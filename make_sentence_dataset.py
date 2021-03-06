
import re
from itertools import groupby
import pandas as pd
import numpy as np
from pathlib import Path
import csv
import random
from datetime import datetime

from datetime import date
today_for_filenames = date.today()
curr_date_out = str(today_for_filenames.strftime("%Y%m%d"))

random.seed(10)

MIN_SENTENCE_LEN = 15



def splitWithIndices(s, c=' '):
    p = 0
    for k, g in groupby(s, lambda x:x==c):
        q = p + sum(1 for i in g)
        if not k:
            yield p, q # or p, q-1 if you are really sure you want that
        p = q

def most_common(lst):
    return max(set(lst), key=lst.count)

def standardize_date(date_str):
    new_date = None
    if not isinstance(date_str, str):
        new_date = date_str
    else:
        space_idx = date_str.find(" ")
        if space_idx == -1:
            new_date = date_str
        else:
            if date_str[space_idx-3] == "/":
                if int(date_str[space_idx-2:space_idx]) < 23:
                    new_date = date_str[:space_idx-2] + "20" + date_str[space_idx-2:]
                else:
                    new_date = date_str[:space_idx-2] + "19" + date_str[space_idx-2:]
            else:
                new_date = date_str

    return new_date

coders_state_translation_dict = {
    "nds":{
        "policy":0,
        "symbolic":1,
        "symbolc":1,
        "smbolic":1,
        "out-geography":4,
        "engagement":99,
        "na":99,
        "NA":99,
        "nan":99,
        "":99,
    },
    "lt":{
        "policy":0,
        "policu":0,
        'symoblic':1,
        'symbolic':1,
        'engagment':99,
        'egnagement':99,
        'engagement':99,
        "na":99,
        "NA":99
    },
    "jw":{
        "policy":0,
        "symbolic":1,
        "constituent":99,
        "contituent":99,
        "consituent":99,
        "contact":99,
        "self-congratulatory":2,
        "representational":2,
        "representation":2,
        "representative":2,
        "empathetic":3,
        "outgroup":4,
        "partisan":5,
        "na":99,
        "NA":99
    }
}
def standardize_annotation(annotation, person):
    annotation = annotation.strip()
    if "/" in annotation:
        annotation = annotation[:annotation.find("/")]
    
    if annotation in coders_state_translation_dict[person]:
        return coders_state_translation_dict[person][annotation]
    else:
        print(annotation)
        return np.nan

def annotation_str_to_list(annotation_str):
    annotation_list = []
    try:
        annotation_list = annotation_str.split(",")
        annotation_list = [elem.strip() for elem in annotation_list]
        return annotation_list
    except:
        return annotation_list

if __name__ == "__main__":

    # Read in newsletter data
    input_csv = "/Users/jacob/Dropbox/GeoAppeals/data/newsletters_clean.csv"
    df = pd.read_csv(input_csv, encoding='iso-8859-1', dtype={"Date":str, "bioguide_id":str, "name.first":str, "name.last":str, "state":str})
    df['Date'] = df['Date'].apply(standardize_date)
    del df['Unnamed: 0']
    df['unique_id'] = df["Date"].map(str) + "::" + df["name.first"].map(str) + "::" + df["name.last"].map(str) + "::" + df["state"].map(str)
    df = (
        df.assign(count=(df["text"].str.len()))
        .sort_values("count")
        .drop_duplicates(subset=['unique_id'], keep="last")
    ).drop('count',axis=1)

    # Read and merge in JW annotations
    jw_annotations_file = "/Users/jacob/Dropbox/GeoAppeals/data/newsletter_samples/coding_sample_may2022_jw.csv"
    jw_df = pd.read_csv(jw_annotations_file, usecols=["Date", "bioguide_id", "name.first", "name.last", "state", "state_annotation"], dtype={"Date":str, "bioguide_id":str, "name.first":str, "name.last":str, "state":str}, encoding="iso-8859-1")
    jw_df['Date'] = jw_df['Date'].apply(standardize_date)
    jw_df = jw_df[jw_df['state_annotation'].notnull()]
    jw_df['unique_id'] = jw_df["Date"].map(str) + "::" + jw_df["name.first"].map(str) + "::" + jw_df["name.last"].map(str) + "::" + jw_df["state"].map(str)
    jw_df = jw_df[["unique_id", "state_annotation"]]
    df = df.merge(jw_df, how="outer", on="unique_id")
    df = df.rename(columns={"state_annotation":"jw_state_annotation"})
    df = df[(df['state.name'].notnull()) & (df['text'].notnull())]
    print(df[df['jw_state_annotation'].notnull()].shape)
    # df[(df['jw_state_annotation'].notnull())].to_csv("/Users/jacob/Downloads/post_all_joins1.csv")

    # Read in LT annotations
    lt_annotations_file = "/Users/jacob/Dropbox/GeoAppeals/data/newsletter_samples/ethom_new100_vfinal.csv"
    lt_df = pd.read_csv(lt_annotations_file, usecols=["Date", "bioguide_id", "name.first", "name.last", "state", "state_annotation"], dtype={"Date":str, "bioguide_id":str, "name.first":str, "name.last":str, "state":str}, encoding='iso-8859-1')
    lt_df['Date'] = lt_df['Date'].apply(standardize_date)
    lt_df = lt_df[lt_df['state_annotation'].notnull()]
    lt_df['unique_id'] = lt_df["Date"].map(str) + "::" + lt_df["name.first"].map(str) + "::" + lt_df["name.last"].map(str) + "::" + lt_df["state"].map(str)
    lt_df = lt_df[["unique_id", "state_annotation"]]
    df = df.merge(lt_df, how="outer", on="unique_id")
    df = df.rename(columns={"state_annotation":"lt_state_annotation"})
    print(df[df['lt_state_annotation'].notnull()].shape)
    # df[(df['jw_state_annotation'].notnull()) | (df['lt_state_annotation'].notnull())].to_csv("/Users/jacob/Downloads/post_all_joins2.csv")

    # Read in NDS annotations
    nds_annotations_file = "/Users/jacob/Dropbox/GeoAppeals/data/newsletter_samples/coding_sample_may2022_noahfinal.csv"
    nds_df = pd.read_csv(nds_annotations_file, usecols=["Date", "bioguide_id", "name.first", "name.last", "state", "state_annotation"], dtype={"Date":str, "bioguide_id":str, "name.first":str, "name.last":str, "state":str}, encoding='iso-8859-1')
    nds_df['Date'] = nds_df['Date'].apply(standardize_date)
    nds_df = nds_df[nds_df['state_annotation'].notnull()]
    nds_df['unique_id'] = nds_df["Date"].map(str) + "::" + nds_df["name.first"].map(str) + "::" + nds_df["name.last"].map(str) + "::" + nds_df["state"].map(str)
    nds_df = nds_df[["unique_id", "state_annotation"]]
    df = df.merge(nds_df, how="outer", on="unique_id")
    df = df.rename(columns={"state_annotation":"nds_state_annotation"})
    print(df[df['nds_state_annotation'].notnull()].shape)
    # df[(df['jw_state_annotation'].notnull()) | (df['lt_state_annotation'].notnull()) | (df['nds_state_annotation'].notnull())].to_csv("/Users/jacob/Downloads/post_all_joins3.csv")


    sentence_level_rows = []
    for row_idx, row in df.iterrows():

        record_state_lower = df.loc[row_idx, 'state.name'].lower()
        record_text = df.loc[row_idx, 'text'].replace("D.C.", "DC")
        record_text_lower = record_text.lower()

        # Get JW's, NDS', and LT's codings, if they exist for the record
        jw_annotations = annotation_str_to_list(row['jw_state_annotation'])
        lt_annotations = annotation_str_to_list(row['lt_state_annotation'])
        nds_annotations = annotation_str_to_list(row['nds_state_annotation'])

        # Iterate over everything I classify as a sentence and identify those w state mentions
        sentence_start_end_indices = list(splitWithIndices(record_text_lower, '.'))
        annotation_idx = 0
        for sentence_idx, sentence_start_end_tuple in enumerate(sentence_start_end_indices):
            sentence_start_idx = sentence_start_end_tuple[0]
            sentence_end_idx = sentence_start_end_tuple[1]
            sentence_original_case = record_text[sentence_start_idx:sentence_end_idx]
            sentence = record_text_lower[sentence_start_idx:sentence_end_idx]

            # skip sentences too short to likely be real sentences
            if len(sentence) < MIN_SENTENCE_LEN:
                continue

            # Check if sentence has a state mention
            if record_state_lower in sentence:

                if len(jw_annotations) == 0 or len(lt_annotations) == 0 or len(nds_annotations) == 0:
                    jw_annotation = np.nan
                    lt_annotation = np.nan
                    nds_annotation = np.nan
                    jw_annotation_standardized = np.nan
                    lt_annotation_standardized = np.nan
                    nds_annotation_standardized = np.nan
                    most_common_annotation = np.nan

                else:
                    jw_annotation = jw_annotations[annotation_idx]
                    lt_annotation = lt_annotations[annotation_idx]
                    nds_annotation = nds_annotations[annotation_idx]
                    
                    if jw_annotations[annotation_idx] == "r" or jw_annotations[annotation_idx] == "r" or jw_annotations[annotation_idx] == "r":
                        print(row)
                        print(list(row))
                        print(jw_annotations)
                        print(lt_annotations)
                        print(nds_annotations)
                        print(annotation_idx)

                    jw_annotation_standardized = standardize_annotation(jw_annotations[annotation_idx], "jw")
                    lt_annotation_standardized = standardize_annotation(lt_annotations[annotation_idx], "lt")
                    nds_annotation_standardized = standardize_annotation(nds_annotations[annotation_idx], "nds")

                    mention_annotations = [jw_annotation_standardized, lt_annotation_standardized, nds_annotation_standardized]
                    mention_annotations = [elem for elem in mention_annotations if not np.isnan(elem) and elem is not None]
                    if len(mention_annotations) > 0:
                        most_common_annotation = most_common(mention_annotations)
                    else:
                        most_common_annotation = np.nan

                    if len(set(mention_annotations)) == 3:
                        most_common_annotation = random.choice(mention_annotations)

                new_row = row.values.flatten().tolist() + [sentence, jw_annotation, lt_annotation, nds_annotation, jw_annotation_standardized, lt_annotation_standardized, nds_annotation_standardized, most_common_annotation]
                sentence_level_rows.append(new_row)

                annotation_idx += 1

    sentence_level_df = pd.DataFrame(sentence_level_rows, columns=list(df.columns) + ["sentence_w_mention", "jw_annotation", "lt_annotation", "nds_annotation", "jw_annotation_standardized", "lt_annotation_standardized", "nds_annotation_standardized", "majority_annotation"])
    print("1: " + str(sentence_level_df.shape))

    sentence_level_df['is_annotated'] = 0
    sentence_level_df.loc[sentence_level_df['majority_annotation'].notnull(), "is_annotated"] = 1
    print("2: " + str(sentence_level_df.shape))
    
    sentence_level_df['temp_unique_id'] = sentence_level_df.index
    sentence_level_df['is_training'] = 0

    annotated_sentences_df = sentence_level_df[sentence_level_df['is_annotated'] == 1]
    print("3: " + str(annotated_sentences_df.shape))

    unique_classes = annotated_sentences_df['majority_annotation'].unique().tolist()
    for unique_class in unique_classes:
        train_annotated_sentences_df_for_class = annotated_sentences_df[annotated_sentences_df['majority_annotation'] == unique_class]
        train_annotated_sentences_df_for_class = train_annotated_sentences_df_for_class.sample(n=int(0.7*(train_annotated_sentences_df_for_class.shape[0])))
        train_col_name = 'is_training_for_' + str(unique_class).replace("/", "_")
        train_annotated_sentences_df_for_class[train_col_name] = 1
        print(train_annotated_sentences_df_for_class.shape[0])
        train_annotated_sentences_df_for_class = train_annotated_sentences_df_for_class[['temp_unique_id', train_col_name]]
        sentence_level_df = sentence_level_df.merge(train_annotated_sentences_df_for_class, how='left', on='temp_unique_id')
        print(sentence_level_df[sentence_level_df[train_col_name] == 1].shape)
        print("=====")
        sentence_level_df.loc[sentence_level_df[train_col_name] == 1, "is_training"] = 1

    # train_annotated_sentences_df = annotated_sentences_df.sample(n=int(annotated_sentences_df.shape[0]*0.7))
    # print("4: " + str(train_annotated_sentences_df.shape))
    # train_annotated_sentences_df['is_training'] = 1
    # train_annotated_sentences_df = train_annotated_sentences_df[['temp_unique_id', 'is_training']]
    # sentence_level_df = sentence_level_df.merge(train_annotated_sentences_df, how='left', on='temp_unique_id')
    print("5: " + str(sentence_level_df.shape))
    sentence_level_df.loc[sentence_level_df['is_training'] != 1, 'is_training'] = 0
    sentence_level_df['is_validation'] = 0
    sentence_level_df.loc[(sentence_level_df['is_annotated'] == 1) & (sentence_level_df['is_training'] == 0), 'is_validation'] = 1
    del sentence_level_df['temp_unique_id']

    sentence_level_df['text'] = sentence_level_df['text'].astype(str)
    sentence_level_df['Date'] = sentence_level_df['Date'].astype(str)
    sentence_level_df['Subject'] = sentence_level_df['Subject'].astype(str)

    sentence_level_df.to_csv("/Users/jacob/Dropbox/GeoAppeals/data/sentence_level_newsletter_dataset_with_annotations_[" + curr_date_out + "].csv", quotechar='"', index=False)
    print(sentence_level_df[sentence_level_df['is_annotated'] == 1].shape)
    print(sentence_level_df[sentence_level_df['is_training'] == 1].shape)
    print(sentence_level_df[sentence_level_df['is_validation'] == 1].shape)
    print(sentence_level_df[sentence_level_df['jw_annotation'].notnull()].shape)
    print(sentence_level_df[sentence_level_df['lt_annotation'].notnull()].shape)
    print(sentence_level_df[sentence_level_df['nds_annotation'].notnull()].shape)
    print(sentence_level_df[sentence_level_df['jw_annotation_standardized'].notnull()].shape)
    print(sentence_level_df[sentence_level_df['lt_annotation_standardized'].notnull()].shape)
    print(sentence_level_df[sentence_level_df['nds_annotation_standardized'].notnull()].shape)

    annotated_df = sentence_level_df[sentence_level_df['is_annotated'] == 1]
    print(pd.crosstab(annotated_df['is_training'], annotated_df['majority_annotation']))


