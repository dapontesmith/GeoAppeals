
from tkinter import *
from tkinter import filedialog
from tkinter import ttk
import re
from itertools import groupby
import pandas as pd
import numpy as np
from pathlib import Path
import csv

MIN_SENTENCE_LEN = 15


# Establish the GUI and set its dimensions
root = Tk()
root.state('zoomed')
root.grid_rowconfigure(0, weight=1)
root.grid_columnconfigure(1, weight=1)
# root.geometry('700x400') # width x height

# Choose file with newsletter data
input_csv = filedialog.askopenfilename()
# read in data here
df = pd.read_csv(str(Path(input_csv)), encoding = "latin1")
# df = pd.read_csv("/Users/jacob/Dropbox/GeoAppeals/data/newsletters_with_state_mentions.csv")
# df = pd.read_csv("C:/Users/dapon/Dropbox/Harvard/GeoAppeals/data/newsletters_with_state_mentions.csv")
# df = pd.read_csv("/Users/elizabeththom/Dropbox/GeoAppeals/data/newsletters_with_state_mentions.csv")
df['state_annotation'] = np.nan
df['DC_annotation'] = np.nan

global_row_idx = 0
initializing_gui = True

def splitWithIndices(s, c=' '):
    p = 0
    for k, g in groupby(s, lambda x:x==c):
        q = p + sum(1 for i in g)
        if not k:
            yield p, q # or p, q-1 if you are really sure you want that
        p = q

def onclick():
   pass

def update_tagging(text, new_global_row_idx):
    global global_row_idx
    global initializing_gui

    if not initializing_gui:
        # Save any data in the state annotation entry field and then clear it
        existing_state_annotation = df.loc[global_row_idx, 'state_annotation']
        new_state_annotation = state_annotation_entry.get()
        try:
            new_state_annotation = str(new_state_annotation)
        except:
            new_state_annotation = ""
        if pd.isnull(existing_state_annotation) and new_state_annotation == "":
            pass
        else:
            df.loc[global_row_idx, 'state_annotation'] = new_state_annotation
        state_annotation_entry.delete(0, END)

        # Save any data in the DC annotation entry field and then clear it
        existing_DC_annotation = df.loc[global_row_idx, 'DC_annotation']
        new_DC_annotation = DC_annotation_entry.get()
        try:
            new_DC_annotation = str(new_DC_annotation)
        except:
            new_DC_annotation = ""
        if pd.isnull(existing_DC_annotation) and new_DC_annotation == "":
            pass
        else:
            df.loc[global_row_idx, 'DC_annotation'] = new_DC_annotation
        DC_annotation_entry.delete(0, END)

        # Get tag names that currently exist and delete all the tags
        tag_names = text.tag_names(index=None)
        for tag_name in tag_names:
            text.tag_remove(tag_name, "1.0", END)

        # increment the global index
        global_row_idx = new_global_row_idx

    # Get the data for the new record to display
    record_state_lower = df.loc[global_row_idx, 'state.name'].lower()
    record_text = df.loc[global_row_idx, 'text'].replace("D.C.", "DC")
    record_text_lower = record_text.lower()

    # Add new record text to the text widget
    text.replace("1.0", END, record_text)

    # Add any existing state mention annotation to the entry widget
    existing_state_annotation = df.loc[global_row_idx, 'state_annotation']
    if pd.isnull(existing_state_annotation):
        existing_state_annotation = ""
    try:
        existing_state_annotation = str(existing_state_annotation)
    except:
        existing_state_annotation = ""
    state_annotation_entry.delete(0, END)
    state_annotation_entry.insert(0, existing_state_annotation)

    # Add any existing DC mention annotation to the entry widget
    existing_DC_annotation = df.loc[global_row_idx, 'DC_annotation']
    if pd.isnull(existing_DC_annotation):
        existing_DC_annotation = ""
    try:
        existing_DC_annotation = str(existing_DC_annotation)
    except:
        existing_DC_annotation = ""
    DC_annotation_entry.delete(0, END)
    DC_annotation_entry.insert(0, existing_DC_annotation)

    # Iterate over everything I classify as a sentence and add & configure tags to highlight sentences and state mentions as relevant
    sentence_start_end_indices = list(splitWithIndices(record_text_lower, '.'))
    for sentence_idx, sentence_start_end_tuple in enumerate(sentence_start_end_indices):
        sentence_start_idx = sentence_start_end_tuple[0]
        sentence_end_idx = sentence_start_end_tuple[1]
        sentence_original_case = record_text[sentence_start_idx:sentence_end_idx]
        sentence = record_text_lower[sentence_start_idx:sentence_end_idx]

        # skip sentences too short to likely be real sentences
        if len(sentence) < MIN_SENTENCE_LEN:
            continue

        # Highlight sentence yellow if it has state mention, or green if it has DC mention. Also highlight green if it has washington mention but only if the state isn't washington. Highlight pink if it has state and DC or (legitimate) washington mention
        should_highlight_sentence_for_state = False
        if record_state_lower in sentence:
            should_highlight_sentence_for_state = True
        should_highlight_sentence_for_DC = False
        if "DC" in sentence_original_case:
            should_highlight_sentence_for_DC = True
        should_highlight_sentence_for_washington = False
        if "washington" in sentence and "washington" not in record_state_lower:
            should_highlight_sentence_for_washington = True

        if should_highlight_sentence_for_state and not should_highlight_sentence_for_DC and not should_highlight_sentence_for_washington:
            sentence_tag_name = "sentence_highlight_" + str(sentence_idx)
            text.tag_add(sentence_tag_name, "1." + str(sentence_start_idx), "1." + str(sentence_end_idx))
            text.tag_config(sentence_tag_name, background="yellow", foreground="black")

        if not should_highlight_sentence_for_state and (should_highlight_sentence_for_DC or should_highlight_sentence_for_washington):
            sentence_tag_name = "sentence_highlight_" + str(sentence_idx)
            text.tag_add(sentence_tag_name, "1." + str(sentence_start_idx), "1." + str(sentence_end_idx))
            text.tag_config(sentence_tag_name, background="green", foreground="black")

        if should_highlight_sentence_for_state and (should_highlight_sentence_for_DC or should_highlight_sentence_for_washington):
            sentence_tag_name = "sentence_highlight_" + str(sentence_idx)
            text.tag_add(sentence_tag_name, "1." + str(sentence_start_idx), "1." + str(sentence_end_idx))
            text.tag_config(sentence_tag_name, background="pink", foreground="black")

        # Highlight each state mention in red
        sentence_part_start_idx = sentence_start_idx
        state_mention_idx = 0
        while True:
            state_relative_idx = record_text_lower[sentence_part_start_idx:sentence_end_idx].find(record_state_lower)
            if state_relative_idx == -1:
                break
            else:
                state_idx = sentence_part_start_idx + state_relative_idx
                state_tag_name = "state_highlight_" + str(sentence_idx) + "-" + str(state_mention_idx)
                text.tag_add(state_tag_name, "1." + str(state_idx), "1." + str(state_idx + len(record_state_lower)))
                text.tag_config(state_tag_name, background="red", foreground="black")

                sentence_part_start_idx = sentence_part_start_idx + state_relative_idx + 1
                state_mention_idx += 1

        # Highlight each DC mention in blue
        sentence_part_start_idx = sentence_start_idx
        DC_mention_idx = 0
        while True:
            DC_relative_idx = record_text[sentence_part_start_idx:sentence_end_idx].find("DC")
            if DC_relative_idx == -1:
                break
            else:
                DC_idx = sentence_part_start_idx + DC_relative_idx
                DC_tag_name = "DC_highlight_" + str(sentence_idx) + "-" + str(DC_mention_idx)
                text.tag_add(DC_tag_name, "1." + str(DC_idx), "1." + str(DC_idx + len("DC")))
                text.tag_config(DC_tag_name, background="blue", foreground="black")

                sentence_part_start_idx = sentence_part_start_idx + DC_relative_idx + 1
                DC_mention_idx += 1

        # Highlight each Washington mention in blue unless the record_state_lower is Washington
        if "washington" not in record_state_lower:
            sentence_part_start_idx = sentence_start_idx
            washington_mention_idx = 0
            while True:
                washington_relative_idx = record_text_lower[sentence_part_start_idx:sentence_end_idx].find("washington")
                if washington_relative_idx == -1:
                    break
                else:
                    washington_idx = sentence_part_start_idx + washington_relative_idx
                    washington_tag_name = "washington_highlight_" + str(sentence_idx) + "-" + str(washington_mention_idx)
                    text.tag_add(washington_tag_name, "1." + str(washington_idx), "1." + str(washington_idx + len("washington")))
                    text.tag_config(washington_tag_name, background="blue", foreground="black")

                    sentence_part_start_idx = sentence_part_start_idx + washington_relative_idx + 1
                    washington_mention_idx += 1


def clicked_prev():
    global global_row_idx
    new_global_row_idx = global_row_idx - 1
    update_tagging(text, new_global_row_idx)

def clicked_next():
    global global_row_idx 
    new_global_row_idx = global_row_idx + 1
    update_tagging(text, new_global_row_idx)

def clicked_save():
    # Save any data in the state mention entry field
    state_annotation = state_annotation_entry.get()
    df.loc[global_row_idx, 'state_annotation'] = state_annotation

    # Save any data in the DC mention entry field
    DC_annotation = DC_annotation_entry.get()
    df.loc[global_row_idx, 'DC_annotation'] = DC_annotation

    print(df.head())
    print(list(df['state_annotation'])[:10])
    print(list(df['DC_annotation'])[:10])

    #save_path = filedialog.asksaveasfile(mode='w', defaultextension=".csv")
    
    save_path = filedialog.asksaveasfilename(defaultextension=".csv")

    print("save path: " + str(save_path))

    rows = []
    rows.append(list(df.columns))
    for idx, row in df.iterrows():
        rows.append(row)

    with open(str(save_path), 'w', errors='ignore') as csv_out:
        csvwriter = csv.writer(csv_out)
        csvwriter.writerows(rows)

    #df.to_csv(save_path, encoding="cp1252")

def clicked_go_to_page():
    page_num_str = page_entry.get()
    page_entry.delete(0, END)
    page_entry.insert(0, "")
    page_num = None
    try:
        page_num = int(page_num_str)
    except:
        pass

    if page_num is not None and page_num >= 0 and page_num <= df.shape[0] - 1:
        global global_row_idx 
        new_global_row_idx = page_num
        update_tagging(text, new_global_row_idx)

# LEFT FRAME
left_frame = Frame(root)
left_frame.grid(column=0, row=0)

# LEFT FRAME UPPER PART
left_frame_upper = Frame(left_frame)
left_frame_upper.grid(column=0, row=0)

# Create box for state-mention annotated text entry with label
state_annotation_label = Label(left_frame_upper, text="State Annotation: ", justify=LEFT)
state_annotation_label.grid(column=0, row=0)
state_annotation_entry = Entry(left_frame_upper)
state_annotation_entry.grid(column=1, row=0)

# Create box for state-mention annotated text entry with label
DC_annotation_label = Label(left_frame_upper, text="DC Annotation: ", justify=LEFT)
DC_annotation_label.grid(column=0, row=1)
DC_annotation_entry = Entry(left_frame_upper)
DC_annotation_entry.grid(column=1, row=1)

# LEFT FRAME SEPARATOR 1
separator1 = ttk.Separator(left_frame, orient='horizontal')
separator1.grid(column=0, row=1, sticky='ew')

# LEFT FRAME MID PART
left_frame_mid = Frame(left_frame)
left_frame_mid.grid(column=0, row=2)

# button widget with red color text inside
prev_btn = Button(left_frame_mid, text = "Prev", fg = "black", command=clicked_prev)
# Set Button Grid
prev_btn.grid(column=0, row=0)

# next button widget with red color text inside
next_btn = Button(left_frame_mid, text = "Next", fg = "black", command=clicked_next)
# Set Button Grid
next_btn.grid(column=1, row=0)

# Create a box for pagination text entry
page_btn = Button(left_frame_mid, text = "Go to Page:", fg = "black", command=clicked_go_to_page)
page_btn.grid(column=0, row=1)

page_entry = Entry(left_frame_mid)
page_entry.grid(column=1, row=1)

# LEFT FRAME SEPARATOR 1
separator2 = ttk.Separator(left_frame, orient='horizontal')
separator2.grid(column=0, row=3, sticky='ew')

# LEFT FRAME LOWER PART
left_frame_lower = Frame(left_frame)
left_frame_lower.grid(column=0, row=4)

# Create save button
save_btn = Button(left_frame_lower, text = "Save", fg = "black", command=clicked_save)
# Set Button Grid
save_btn.grid(column=0, row=0)

# RIGHT FRAME
right_frame = Frame(root)
right_frame.grid_rowconfigure(0, weight=1)
right_frame.grid_columnconfigure(0, weight=1)
right_frame.grid(column=1, row=0, sticky='nsew')

# create text widget for displaying newsletter text
text = Text(right_frame)
text.insert(INSERT, "Initial text.")
update_tagging(text, 0)
initializing_gui = False
text.grid(column=0, row=0, sticky='nsew')


root.mainloop()


