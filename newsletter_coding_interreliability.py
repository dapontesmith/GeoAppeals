import csv
from collections import Counter
from sklearn.metrics import cohen_kappa_score
import re
from nltk import agreement

NUM_RECORDS_CODABLE = 499
BASE_DIR = "/Users/jacob/Dropbox/GeoAppeals/data/newsletter_samples/"
if __name__ == "__main__":

	coders = ["jw", "lt", "nds"]
	coders_state_translation_dict = {
		"nds":{
			"policy":0,
			"symbolic":1,
			"symbolc":1,
			"engagement":2,
			"na":99,
			"nan":99
		},
		"lt":{
			"policy":0,
			"policu":0,
			'symoblic':1,
			'symbolic':1,
			'engagment':2,
			'egnagement':2,
			'engagement':2,
			"na":99
		},
		"jw":{
			"policy":0,
			"symbolic":1,
			"constituent":2,
			"contituent":2,
			"consituent":2,
			"contact":2,
			"self-congratulatory":2,
			"representational":2,
			"representation":2,
			"representative":2,
			"empathetic":2,
			"outgroup":3,
			"partisan":4,
			"na":99,
		}
	}
	# "policy/symbolic":0,
	# "policy/constituent":0,
	# "policy/representative":0,
	# "symbolic/policy":1,
	# "symbolic/constituent":1,
	# "symbolic/representative":1,
	# "constituent/policy":2,
	# "constituent/symbolic":2,
	# "constituent/representative":2,
	# "representative/policy":3,
	# "representative/symbolic":3,
	# "representative/constituent":3,
	# "representational/policy":3,
	# "representational/symbolic":3,
	# "representational/constituent":3
	coders_dc_translation_dict = {
		"nds":{},
		"lt":{},
		"jw":{
			"policy":0,
			"federal":1,
			"constituent":2,
			"outgroup":3,
			"na":99
		}
	}

	coder_csvs_dict = {
		"jw":[BASE_DIR + "coding_sample_joint_jw_v7.csv"],
		"nds":[BASE_DIR + "coding_sample_joint_noah_full.csv"],
		"lt":[BASE_DIR + "updated_coding_sample_joint_ET.csv"],
	}
	coder_all_state_annotations_dict = {
		"nds":[],
		"jw":[],
		"lt":[]
	}
	coder_all_dc_annotations_dict = {
		"nds":[],
		"jw":[],
		"lt":[]
	}

	coder_complete_datasets_dict = {}
	headers_dict = {
		"nds": None,
		"jw": None,
		"lt": None,		
	}


	# Read in the annotated csvs
	for coder in coders:
		coder_csvs = coder_csvs_dict[coder]
		coder_complete_data = []
		for coder_csv_idx, coder_csv in enumerate(coder_csvs):

			with open(coder_csv, "r", errors='ignore') as csv_in:
				csvreader = csv.reader(csv_in)
				
				header = None
				state_annotation_idx = None
				dc_annotation_idx = None
				isHeader = True
				row_idx = 0
				for row in csvreader:
					if all([len(elem) == 0 for elem in row]):
						continue
					if isHeader:
						header = row
						state_annotation_idx = header.index("state_annotation")
						dc_annotation_idx = header.index("DC_annotation")
						headers_dict[coder] = header
						isHeader = False
						continue

					state_annotation = row[state_annotation_idx].lower()
					state_annotation = state_annotation.replace(", ", ",")
					state_annotation = re.sub(" [\(\[].*?[\)\]]", "", state_annotation) # Get rid of any parentheticals
					state_annotation = re.sub("/[^,]*,", ",", state_annotation) # Get rid of any secondary codings (applies to all but last annotation)
					state_annotation = re.sub("/[^,]*,?", "", state_annotation).strip() # Get rid of any secondary codings (only applies to last annotation)
					state_annotation_list = re.split("\s|,", state_annotation)
					# state_annotation_list = state_annotation.split(",")
					state_annotation_list = [elem.strip() for elem in state_annotation_list]
					coder_all_state_annotations_dict[coder] = coder_all_state_annotations_dict[coder] + state_annotation_list
					coder_state_translation_dict = coders_state_translation_dict[coder]
					translated_state_annotation_list = [coder_state_translation_dict[annotation] if annotation in coder_state_translation_dict else -1 for annotation in state_annotation_list]

					dc_annotation = row[dc_annotation_idx].lower()
					dc_annotation = dc_annotation.replace(", ", ",")
					dc_annotation = re.sub("[\(\[].*?[\)\]]", "", dc_annotation)
					dc_annotation = re.sub("/[^,]*,", ",", dc_annotation) # Get rid of any secondary codings (applies to all but last annotation
					dc_annotation = re.sub("/[^,]*,?", "", dc_annotation) # Get rid of any secondary codings (only applies to last annotation)
					dc_annotation_list = re.split("\s|,", dc_annotation)
					# dc_annotation_list = dc_annotation.split(",")
					dc_annotation_list = [elem.strip() for elem in dc_annotation_list]
					coder_all_dc_annotations_dict[coder] = coder_all_dc_annotations_dict[coder] + dc_annotation_list
					coder_dc_translation_dict = coders_dc_translation_dict[coder]
					translated_dc_annotation_list = [coder_dc_translation_dict[annotation] if annotation in coder_dc_translation_dict else -1 for annotation in dc_annotation_list]
					
					new_row = row + [translated_state_annotation_list, translated_dc_annotation_list]

					if coder_csv_idx == 0:
						coder_complete_data.append(new_row)
					else:
						if not all([elem == "" or elem == -1 for elem in translated_state_annotation_list]):
							coder_complete_data[row_idx] = new_row
	
					row_idx += 1

				print("Len of: " + str(len(coder_complete_data)))
				print("coder idx: " + str(coder_csv_idx))

		coder_complete_datasets_dict[coder] = coder_complete_data

	print("NDS state codes: " + str(set(coder_all_state_annotations_dict["nds"])))
	print("LT state codes: " + str(set(coder_all_state_annotations_dict["lt"])))
	print("JW state codes: " + str(set(coder_all_state_annotations_dict["jw"])))

	print("NDS DC codes: " + str(set(coder_all_dc_annotations_dict["nds"])))
	print("LT DC codes: " + str(set(coder_all_dc_annotations_dict["lt"])))
	print("JW DC codes: " + str(set(coder_all_dc_annotations_dict["jw"])))

	print("Num records NDS: " + str(len(coder_complete_datasets_dict["nds"])))
	print("Num records LT: " + str(len(coder_complete_datasets_dict["lt"])))
	print("Num records JW: " + str(len(coder_complete_datasets_dict["jw"])))






	coder_idx_dict = {
		"nds":0,
		"lt":1,
		"jw":2
	}
	state_annotation_taskdata_for_nltk = []
	dc_annotation_taskdata_for_nltk = []
	annotation_idx_dict = {"jw": 0, "lt": 0, "nds": 0}
	num_records_with_coding_difference = 0
	num_mentions_all_agree = 0
	num_mentions_at_least_two_agree = 0
	num_mentions = 0
	single_disagreement_counter = {"jw-lt":0, "jw-nds":0, "lt-nds":0}
	single_disagreement_counter_disagg = {"jw-lt":{}, "jw-nds":{}, "lt-nds":{}}
	for i in range(NUM_RECORDS_CODABLE):
		record_coded_by_all = True
		differing_lengths = False
		task_completed_by_all_coders = []
		coders_annotations = []
		coders_coded_newsletter = []
		num_state_annotations = None
		for coder in coder_complete_datasets_dict:
			coder_idx = coder_idx_dict[coder]
			coder_complete_data = coder_complete_datasets_dict[coder]
			record = coder_complete_data[i]

			newsletter = record[headers_dict[coder].index("text")]
			coders_coded_newsletter.append(newsletter)
			state_annotations = record[-2]
			coders_annotations.append((coder, state_annotations))
			# print(coder + " : " + str(state_annotations))
			if num_state_annotations is None:
				num_state_annotations = len(state_annotations)
			else:
				if num_state_annotations != len(state_annotations):
					# print("differing lengs")
					differing_lengths = True
					# record_coded_by_all = False

			if len(state_annotations) == 0 or all([state_annotation == -1 for state_annotation in state_annotations]):
				# print("No annotations")
				record_coded_by_all = False

			for state_annotation in state_annotations:
				if state_annotation == "":
					continue
				state_annotation_tuple_for_nltk = (coder_idx, str(annotation_idx_dict[coder]), str(state_annotation))
				annotation_idx_dict[coder] += 1
				task_completed_by_all_coders.append(state_annotation_tuple_for_nltk)
			
		if differing_lengths and record_coded_by_all:
			print("===================")
			print("Differing record: " + str(i))
			print(coders_annotations[0][0] + " : " + str(coders_annotations[0][1]))
			print(coders_annotations[1][0] + " : " + str(coders_annotations[1][1]))
			print(coders_annotations[2][0] + " : " + str(coders_annotations[2][1]))
			print(coders_coded_newsletter[0])
			print(coders_coded_newsletter[1])
			print(coders_coded_newsletter[2])
			print("-------------------")

		if record_coded_by_all:
			state_annotation_taskdata_for_nltk = state_annotation_taskdata_for_nltk + task_completed_by_all_coders

			num_annotations_in_record = len(coders_annotations[0][1])
			coding_difference = True
			for j in range(num_annotations_in_record):
				code_str = str(coders_annotations[0][1][j]) + "::" + str(coders_annotations[1][1][j]) + "::" + str(coders_annotations[2][1][j])
				if coders_annotations[0][1][j] == coders_annotations[1][1][j] and coders_annotations[0][1][j] == coders_annotations[2][1][j]:
					coding_difference = False
					num_mentions_all_agree += 1
				agreement_score = 2*(coders_annotations[0][1][j] == coders_annotations[1][1][j]) + 2*(coders_annotations[0][1][j] == coders_annotations[2][1][j]) + 2*(coders_annotations[1][1][j] == coders_annotations[2][1][j])
				if agreement_score > 0:
					num_mentions_at_least_two_agree += 1
				if len({coders_annotations[0][1][j], coders_annotations[1][1][j], coders_annotations[2][1][j]}) != 1 and agreement_score > 0:
					if coders_annotations[0][1][j] == coders_annotations[1][1][j]:
						single_disagreement_str = "jw-lt"
					elif coders_annotations[0][1][j] == coders_annotations[2][1][j]:
						single_disagreement_str = "jw-nds"
					elif coders_annotations[1][1][j] == coders_annotations[2][1][j]:
						single_disagreement_str = "lt-nds"
					
					single_disagreement_counter[single_disagreement_str] += 1
					if code_str in single_disagreement_counter_disagg[single_disagreement_str]:
						single_disagreement_counter_disagg[single_disagreement_str][code_str] += 1
					else:
						single_disagreement_counter_disagg[single_disagreement_str][code_str] = 0

			if coding_difference:
				print(coders_annotations)
				print(coders_annotations[0])
				print(coders_annotations[1])
				print(coders_annotations[2])
				print("*****************")
				num_records_with_coding_difference += 1
			else:
				pass

			num_mentions += num_annotations_in_record


			# print("Not Missing")

			# dc_annotations = record[-1]
			# for dc_annotation in dc_annotations:
			# 	if dc_annotation == "":
			# 		continue
			# 	dc_annotation_tuple_for_nltk = (coder_idx, str(record_idx), str(dc_annotation))
			# 	dc_annotation_taskdata_for_nltk.append(dc_annotation_tuple_for_nltk)

		# print("Len taskdata: " + str(len(state_annotation_taskdata_for_nltk)))

	print("Num mentions all agree: " + str(num_mentions_all_agree))
	print("Num mentions 2+ agree: " + str(num_mentions_at_least_two_agree))
	print("Num total mentions: " + str(num_mentions))
	print("Overall Odd-person-out breakdown:")
	print(single_disagreement_counter)
	print("Disaggregated Odd-person-out breakdown:")
	print(single_disagreement_counter_disagg)

	print("State interreliability scores")
	print("Num mentions coded by all: " + str(len(state_annotation_taskdata_for_nltk)))
	ratingtask = agreement.AnnotationTask(data=state_annotation_taskdata_for_nltk)
	print("kappa " +str(ratingtask.kappa()))
	print("fleiss " + str(ratingtask.multi_kappa()))
	print("alpha " +str(ratingtask.alpha()))
	print("scotts " + str(ratingtask.pi()))

	# print("DC interreliability scores")
	# ratingtask = agreement.AnnotationTask(data=dc_annotation_taskdata_for_nltk)
	# print("kappa " +str(ratingtask.kappa()))
	# print("fleiss " + str(ratingtask.multi_kappa()))
	# print("alpha " +str(ratingtask.alpha()))
	# print("scotts " + str(ratingtask.pi()))




