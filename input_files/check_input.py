#!/usr/bin/ Python
#
# Purpose:
#   Check input files for IWC SC67b Gray Whale Rangewide Model.
# Notes:
#   - Checks values for trial factors against specification table
#     ('Trials Table.xlsx' from AEP)
#   - This script should reside in the same directory as the input files
#     to be verified.
#   - Additionally, the file 'design_matrix.csv' should reside
#     in the same directory.
# Author:
#   John Brandon
from glob import glob as ls  # for listing files in a directory
import csv                   # for reading *.csv files

code_list = []               # initialize empty list for trial codes
input_dict = dict()          # '...' dictionary for input file parameters
spec_dict = dict()           # '...' for parameters in specifications

file_list = ls('COPYGW*')    # return list containing relevant input file names
print 'There are %s input files in the validation set.' % len(file_list)

# Extract trial codes, e.g., ['3a0', '5a0', ...] from input file names
#   Then extract parameter values from input files corresponding
#   w trial factor specs table
for f in file_list:
	trial_i = f.split('.')[0][6:]
	code_list.append(trial_i)
	input_dict[trial_i] = []  # init empty list in the dictionary for each trial
	file_i = open(f)
	for line_num, input_string in enumerate(file_i):
		if line_num == 98:                 # WFG in BSCS
			wfg_bscs = float(input_string.rstrip()[39:40])  # .rstrip() \r\n etc.
			input_dict[trial_i].append((wfg_bscs))
		if line_num == 100:                # PCFG in BSCS
			pcfg_bscs = float(input_string.rstrip()[39:40])
			input_dict[trial_i].append(pcfg_bscs)
		if line_num in range(17, 21):      # MSYR option for fixed or estimated value
			msyr_opt = float(input_string.rstrip()[47:])
			input_dict[trial_i].append(msyr_opt)
		if line_num == 81:                 # MSYR Values for WBG, WFG, NFG, PCFG
			msyr_vals = input_string.rstrip()[54:].split('   ')
			msyr_vals = [s for s in msyr_vals if s != '']  # catch odd white spaces
			msyr_vals = [float(i) for i in msyr_vals]      # coerce each list element to float
			input_dict[trial_i].extend(msyr_vals)          # note `extend` not `append`
		if line_num == 41:                 # bycatch multiplier
			dead_multiplier = float(input_string.rstrip()[45:])
			input_dict[trial_i].append(dead_multiplier)
		if line_num in range(66, 68):  # Pulse size (also check two values are equal)
			if line_num == 66:
				pulse_size = set()
			pulse_size.add(float(input_string.rstrip()[21:]))
			if line_num == 67:
				if len(pulse_size) == 1:
					input_dict[trial_i].append(pulse_size.pop())
				else:
					raise Exception('ERROR: Pulse sizes not equal between years. TRIAL %s' % trial_i)
		if input_string.rstrip() == '# Year1 Year2 Stock   Rate  SD':
			imm_string = next(enumerate(file_i))[1].rstrip()
			input_dict[trial_i].append(float(imm_string[22:27]))

# Now we have a dictionary with each trial code representing a key, and the
#  values for each factor within that trial corresponding with the specs. The values are 
#  represented in a list that completes each key:value component of the dictionary, e.g:
# input_dict = {'trial_keycode':['list', 'of', 'values'],
# 				'3a0':[0,0,0,0,4,20, etc.],
#				'3a1':[1,1,1,1,4,20, etc.]
# 				}

# Read values from the specs for comparison ------------------------------------
# The next step is to read a second dictionary of key:value components from the 
#   specs. Once the information is stored in this structure, making a comparison 
#   between the input files and specs can be completed.
print ''
print 'reading specification matrix for verification'
specs = dict()
spec_codes = []  # list of code keys in (row) order they are read in from specs matrix
params = ['msyr_opt1', 'msyr_opt2', 'msyr_opt3', 'msyr_opt4', 'dead_multiplier',
		  'pulse_size', 'msyr_wbg', 'msyr_wfg',	'msyr_nfg', 'msyr_pcfg', 'wfg_bcsc',
		  'pcfg_bcsc', 'imm_rate']
with open('specs_matrix.csv') as csvfile:
	reader = csv.DictReader(csvfile)
	for trial_specs in reader:  # run through each row (trial) in specs design matrix
		code_key = trial_specs.pop('code')  # trial specs is dictionary with {param1:value1, param2:value2} 
		spec_codes.append(code_key)
		specs[code_key] = []  # set key as trial code and initialize empty list as value
		for param in params: # iterate over key:value pairs
			specs[code_key].append(float(trial_specs[param]))

# Save input_dict by writing it to a file --------------------------------------
print 'output specifications retrieved from input files'
print 'saving parameters from input files to spec_matrix.in'
f_out = open('spec_matrix.in', 'w')
for code_key in spec_codes:
	f_out.write(str(code_key) + ' ' + str(input_dict[code_key]) + '\n')
f_out.close()

# Element wise comparison of trial lists within the dictionaries retrieved 
#   from the input (COPYGW*.dat) files and the 'Trial Tables' specs (specs_matrix.csv)
f_err = open('err_report.txt', 'w')
params = ['msyr_opt1', 'msyr_opt2', 'msyr_opt3', 'msyr_opt4', 'dead_multiplier',
		  'pulse_size', 'msyr_wbg', 'msyr_wfg',	'msyr_nfg', 'msyr_pcfg', 'wfg_bcsc',
		  'pcfg_bcsc', 'imm_rate']
for trial in spec_codes:
	for i, input_val in enumerate(input_dict[trial]):
		if input_val != specs[trial][i]:                               # trouble
			# print trial, i, input_val, specs[trial][i]
			err_msg = 'Mismatch in trial %s: param = %s --> input_file = %s vs. trial_table = %s'\
				% (str(trial), str(params[i]), str(input_val), str(specs[trial][i]))
			f_err.write(err_msg + '\n')
f_err.close()			
