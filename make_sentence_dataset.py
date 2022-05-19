
import re
from itertools import groupby
import pandas as pd
import numpy as np
from pathlib import Path
import csv
import random

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

if __name__ == "__main__":

    # Read in newsletter data
    input_csv = "/Users/jacob/Dropbox/GeoAppeals/data/newsletters_clean.csv"
    df = pd.read_csv(input_csv, encoding='iso-8859-1')
    df['unique_id'] = df["Date"].astype(str) + "::" + df["name.first"] + "::" + df["name.last"] + "::" + df["state"]

    # Read in JW annotations
    jw_annotations_file = "/Users/jacob/Dropbox/GeoAppeals/data/newsletter_samples/coding_sample_may2022_jw.csv"
    jw_df = pd.read_csv(jw_annotations_file, usecols=["Date", "bioguide_id", "name.first", "name.last", "state", "state_annotation"], encoding="iso-8859-1")
    jw_df['unique_id'] = jw_df["Date"].astype(str) + "::" + jw_df["name.first"].map(str) + "::" + jw_df["name.last"].map(str) + "::" + jw_df["state"].map(str)
    print(jw_df['unique_id'].value_counts())
    print("=========")
    jw_df = jw_df[["unique_id", "state_annotation"]]
    df = df.merge(jw_df, how="outer", on="unique_id")
    df = df.rename(columns={"state_annotation":"jw_state_annotation"})

    # Remove records where the state column or text column is NaN
    df['jw_state_annotation'] = df['jw_state_annotation'].astype(str)
    df.loc[df['jw_state_annotation'] == "[]", 'jw_state_annotation'] = np.nan
    print(df['jw_state_annotation'].value_counts())
    print(df[~df['jw_state_annotation'].notnull()].shape)
    df = df[(~df['state.name'].notnull()) & (~df['text'].notnull())]
    print(df[~df['jw_state_annotation'].notnull()].shape)
    exit()

    # Read in LT annotations
    lt_annotations_file = "/Users/jacob/Dropbox/GeoAppeals/data/newsletter_samples/ethom_new100_vfinal.csv"
    lt_df = pd.read_csv(lt_annotations_file, usecols=["Date", "bioguide_id", "name.first", "name.last", "state", "state_annotation"], encoding='iso-8859-1')
    lt_df['unique_id'] = lt_df["Date"].astype(str) + "::" + lt_df["name.first"].map(str) + "::" + lt_df["name.last"].map(str) + "::" + lt_df["state"].map(str)
    lt_df = lt_df[["unique_id", "state_annotation"]]
    df = df.merge(lt_df, how="outer", on="unique_id")
    df = df.rename(columns={"state_annotation":"lt_state_annotation"})

    # Read in NDS annotations
    nds_annotations_file = "/Users/jacob/Dropbox/GeoAppeals/data/newsletter_samples/coding_sample_may2022_noahfinal.csv"
    nds_df = pd.read_csv(nds_annotations_file, usecols=["Date", "bioguide_id", "name.first", "name.last", "state", "state_annotation"], encoding='iso-8859-1')
    nds_df['unique_id'] = nds_df["Date"].map(str) + "::" + nds_df["name.first"].map(str) + "::" + nds_df["name.last"].map(str) + "::" + nds_df["state"].map(str)
    nds_df = nds_df[["unique_id", "state_annotation"]]
    df = df.merge(nds_df, how="outer", on="unique_id")
    df = df.rename(columns={"state_annotation":"nds_state_annotation"})

    sentence_level_rows = []
    for row_idx, row in df.iterrows():

        record_state_lower = df.loc[row_idx, 'state.name'].lower()
        record_text = df.loc[row_idx, 'text'].replace("D.C.", "DC")
        record_text_lower = record_text.lower()

        # Get JW's, NDS', and LT's codings, if they exist for the record
        jw_annotations = row['jw_state_annotation']
        lt_annotations = row['lt_state_annotation']
        nds_annotations = row['nds_state_annotation']

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

                if pd.isnull(jw_annotations) or pd.isnull(lt_annotations) or pd.isnull(nds_annotations):
                    jw_annotation = np.nan
                    lt_annotation = np.nan
                    nds_annotation = np.nan
                    most_common_annotation = np.nan

                else:
                    jw_annotation = jw_annotations[annotation_idx]
                    lt_annotation = lt_annotations[annotation_idx]
                    nds_annotation = nds_annotations[annotation_idx]

                    mention_annotations = [jw_annotation, lt_annotation, nds_annotation]
                    most_common_annotation = most_common(mention_annotations)
                    if len(set(mention_annotations)) == 3:
                        most_common_annotation = random.choice(mention_annotations)

                new_row = row.values.flatten().tolist() + [sentence, jw_annotation, lt_annotation, nds_annotation, most_common_annotation]
                sentence_level_rows.append(new_row)

                annotation_idx += 1

    sentence_level_df = pd.DataFrame(sentence_level_rows, columns=list(df.columns) + ["sentence_w_mention", "jw_annotation", "lt_annotation", "nds_annotation", "majority_annotation"])
    
    sentence_level_df['is_annotated'] = 0
    sentence_level_df.loc[~sentence_level_df['majority_annotation'].isnull(), "is_annotated"] = 1
    print(sentence_level_df.shape)
    
    sentence_level_df['is_training'] = 0
    sentence_level_df['is_validation'] = 0
    sentence_level_df['temp_unique_id'] = sentence_level_df.index

    annotated_sentences_df = sentence_level_df[sentence_level_df['is_annotated'] == 1]
    print(annotated_sentences_df.shape)
    train_annotated_sentences_df = annotated_sentences_df.sample(n=int(sentence_level_df.shape[0]*0.7))
    train_annotated_sentences_df['is_training'] = 1
    sentence_level_df = sentence_level_df.merge(train_annotated_sentences_df, how='left', on='temp_unique_id')
    sentence_level_df.loc[(sentence_level_df['is_annotated'] == 1) & (sentence_level_df['is_training'] == 0), 'is_validation'] = 1
    del sentence_level_df['temp_unique_id']

    sentence_level_df.to_csv("/Users/jacob/Dropbox/GeoAppeals/data/sentence_level_newsletter_dataset_with_annotations.csv")

