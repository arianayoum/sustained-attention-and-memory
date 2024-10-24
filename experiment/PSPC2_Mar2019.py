from __future__ import division
import numpy as np
import pandas as pd
from pandas import Series, DataFrame
import time, random, os, csv
from random import shuffle, sample
from psychopy import visual, gui, core, data, tools, event, logging
from psychopy.tools.filetools import fromFile, toFile
from datetime import datetime
from scipy.stats import norm
from psychopy.preferences import prefs

# experiment name 
expName = 'PS'

# Prompt pop-up
expInfo = {'participant ID': '', 'session': '001'}

# subject info
dlg = gui.DlgFromDict(dictionary=expInfo, title=expName) 
if not dlg.OK:
    core.quit() 

# Get the filename based on participant number
fileName = expInfo['participant ID']

# Set directory
dir = os.path.dirname(os.path.abspath("PSPC2_Mar2019.py"))
os.chdir(dir)

# Create file

os.chdir(os.path.join(dir, fileName))

f5 = open(fileName+'_PSretrieval.csv', 'w')
f5.write('subject, condition, word, answer, accuracy, RT')

# Create a window
mywin = visual.Window([1920,1080], allowGUI=False, monitor='testMonitor', units='deg', fullscr=True)
mywin.recordFrameIntervals = True
mywin.refreshThreshold = 1/60 + 0.004

# Practice stimuli
os.chdir(os.path.join(dir, 'Stim'))

prac = pd.read_csv("example.csv")
prac_encoding = prac["Word"]
prac_stem = prac["Stem"]
prac_PS = prac["Syn"]

# Messages
message2 = visual.TextStim(mywin, pos=[0,-3], text='Hit any key to begin.', height=1)
feedback1 = visual.TextStim(mywin, text='Great Work! Do you have any questions?')

###################################################################################################
##################################### PS Stimuli Setup ############################################
###################################################################################################

# Practice stimuli
os.chdir(os.path.join(dir, 'Stim'))

prac = pd.read_csv("example.csv")
prac_encoding = prac["Word"]
prac_stem = prac["Stem"]
prac_PS = prac["Syn"]

# Setup stimuli
files = (['C','D','E', 'F', 'G', 'H'], ['D','E','F', 'G', 'H', 'A'], 
['E','F','G', 'H', 'A', 'B'], ['F','G','H', 'A', 'B', 'C'], ['G','H','A', 'B', 'C', 'D'], 
['H','A','B', 'C', 'D', 'E'], ['A','B','C', 'D', 'E', 'F'], ['B','C','D', 'E', 'F', 'G'])
x = int(expInfo['participant ID'])

if x%8 == 0:
    set = 7
else:
    set = (x%8)-1

bin1 = files[set][0] # block1 word
bin2 = files[set][1] # block1 lure 
bin3 = files[set][2] # block1 new
bin4 = files[set][3] # block2 word
bin5 = files[set][4] # block2 lure
bin6 = files[set][5] # block2 new

Bin1= pd.read_csv("Bin" + bin1 + ".csv")
Bin1= Bin1.sample(frac=1).reset_index(drop=True)
Bin2= pd.read_csv("Bin" + bin2 + ".csv")
Bin2= Bin2.sample(frac=1).reset_index(drop=True)
Bin3= pd.read_csv("Bin" + bin3 + ".csv")
Bin3= Bin3.sample(frac=1).reset_index(drop=True)
Bin4= pd.read_csv("Bin" + bin4 + ".csv")
Bin4= Bin4.sample(frac=1).reset_index(drop=True)
Bin5= pd.read_csv("Bin" + bin5 + ".csv")
Bin5= Bin5.sample(frac=1).reset_index(drop=True)
Bin6= pd.read_csv("Bin" + bin6 + ".csv")
Bin6= Bin6.sample(frac=1).reset_index(drop=True)

# Block 1 encoding 

stim = pd.concat([Bin1, Bin2, Bin4, Bin5], ignore_index=True)
stim = stim.sample(frac=1).reset_index(drop=True)

# Block 1 retrieval

target1 = Bin1["Word"]
lure1 = Bin2["Syn"]
new1 = Bin3["Word"]
target2 = Bin4["Word"]
lure2 = Bin5["Syn"]
new2 = Bin6["Word"]

allstim = pd.Series()
allstim = allstim.append([target1, lure1, new1, target2, lure2, new2], ignore_index=True)
allstim = allstim.to_frame()
allstim.columns = ['Word']
allstim['Condition'] = ''
allstim.Condition[0:20] = 'Old'
allstim.Condition[20:40] = 'Lure'
allstim.Condition[40:60] = 'New'
allstim.Condition[60:80] = 'Old'
allstim.Condition[80:100] = 'Lure'
allstim.Condition[100:120] = 'New'

allstim = allstim.sample(frac=1).reset_index(drop=True)
allwords = allstim['Word']

sans = ['Helvetica']
fixation = visual.GratingStim(mywin, color=-1, colorSpace='rgb', tex=None, mask='cross', size=0.6)

###################################################################################################
######################################### PS Retrieval ############################################
###################################################################################################

# timing
ntr = 120
stim_length = 2
trial_length = stim_length + 0.5
exp_length = ntr*trial_length + 0.5

# Visual keys
os.chdir(os.path.join(dir, 'Keys'))

old = visual.ImageStim(mywin, image = 'old.png', pos=[-0.3,-0.5], units='norm', size = (0.2,0.3))
new = visual.ImageStim(mywin, image = 'new.png', pos=[0.3,-0.5], units='norm', size = (0.2,0.3))
oldpress = visual.ImageStim(mywin, image = 'oldpress.png', pos=[-0.3,-0.5], units='norm', size = (0.2,0.3))
newpress = visual.ImageStim(mywin, image = 'newpress.png', pos=[0.3,-0.5], units='norm', size = (0.2,0.3))

# Display Instructions
os.chdir(os.path.join(dir, 'Instructions'))

for i in np.arange(25,32):
    inst = visual.ImageStim(mywin, image='Slide'+str(i)+'.png', pos=[0,0],units='norm', size=2)
    inst.draw()
    mywin.flip()
    core.wait(0.5)
    event.waitKeys()

# Practice 
expClock = core.Clock()
trialClock = core.Clock()

os.chdir(dir)

for word in prac_PS:
    message = visual.TextStim(mywin, text=word, height=3)
    message.draw()
    old.draw()
    new.draw()
    mywin.flip()
    allKeys=event.waitKeys(maxWait=2, keyList=["b","n",'q'],timeStamped=trialClock)
    if allKeys!= None:
        for thisKey in allKeys:
            if thisKey[0] == 'b':
                message.draw()
                new.draw()
                oldpress.draw()
                mywin.flip()
                core.wait(0.1)
            if thisKey[0] == 'n':
                message.draw()
                old.draw()
                newpress.draw()
                mywin.flip()
                core.wait(0.1)
            elif thisKey[0] in ['q', 'escape']:
                win.close()
                core.quit() #abort experiment
        fixation.draw()
        mywin.flip()
        core.wait(0.5)
        event.clearEvents()
    else:
        fixation.draw()
        old.draw()
        new.draw()
        mywin.flip()
        allKeys=event.waitKeys(maxWait=0.5, keyList=["b","n",'q'],timeStamped=trialClock)
        if allKeys!= None:
            for thisKey in allKeys:
                if thisKey[0] == 'b':
                    new.draw()
                    oldpress.draw()
                    mywin.flip()
                    core.wait(0.1)
                if thisKey[0] == 'n':
                    old.draw()
                    newpress.draw()
                    mywin.flip()
                    core.wait(0.1)
                elif thisKey[0] in ['q', 'escape']:
                    win.close()
                    core.quit() #abort experiment
    event.clearEvents()

#give some on screen feedback

feedback1.draw()
message2.draw()
mywin.flip()
event.waitKeys()

# Task Instructions
message5 = visual.TextStim(mywin, pos=[0,+3], text='For each word, indicate if OLD (B) or NEW (N).')
message6 = visual.TextStim(mywin, pos=[0,-3], text='Hit any key to begin.')
message5.draw()
message6.draw()
fixation.draw()
mywin.flip()
#key press
event.waitKeys()
expClock = core.Clock()

# draw stimuli

for word in allwords:
    message = visual.TextStim(mywin, text=word, height=3)
    message.draw()
    old.draw()
    new.draw()
    mywin.flip()
    trialClock = core.Clock()
    # clear event space
    event.clearEvents()
    accuracy = 999
    allKeys=event.waitKeys(maxWait=2, keyList=["b","n",'q'],timeStamped=trialClock)
    if allKeys!= None:
        for thisKey in allKeys:
            rt = thisKey[1]
            i = (allstim.Word[allstim.Word == str(word)].index.tolist())[0]
            if thisKey[0] == 'b':
                message.draw()
                new.draw()
                oldpress.draw()
                mywin.flip()
                core.wait(0.1)
                answer = 'Old'
                if (thisKey[0]=='b') & (allstim.Condition[i]=='Old'): 
                    accuracy = 1 
                else: 
                    accuracy = 0
            if thisKey[0] == 'n':
                message.draw()
                old.draw()
                newpress.draw()
                mywin.flip()
                core.wait(0.1)
                answer = 'New'
                if (thisKey[0]=='n') & (allstim.Condition[i]=='New'): 
                    accuracy = 1 
                elif (thisKey[0]=='n') & (allstim.Condition[i]=='Lure'): 
                    accuracy = 1  
                else: 
                    acccuracy = 0
            elif thisKey[0] in ['q', 'escape']:
                win.close()
                core.quit() #abort experiment
        fixation.draw()
        core.wait(0.5)
        mywin.flip()
        event.clearEvents()
    else:
        fixation.draw()
        old.draw()
        new.draw()
        mywin.flip()
        allKeys=event.waitKeys(maxWait=0.5, keyList=["b","n",'q'],timeStamped=trialClock)
        if allKeys!= None:
            for thisKey in allKeys:
                rt = thisKey[1]
                i = (allstim.Word[allstim.Word == str(word)].index.tolist())[0]
                if thisKey[0] == 'b':
                    new.draw()
                    oldpress.draw()
                    mywin.flip()
                    core.wait(0.1)
                    answer = 'Old'
                    if (thisKey[0]=='b') & (allstim.Condition[i]=='Old'): 
                        accuracy = 1 
                    else: 
                        accuracy = 0
                if thisKey[0] == 'n':
                    old.draw()
                    newpress.draw()
                    mywin.flip()
                    core.wait(0.1)
                    answer = 'New'
                    if (thisKey[0]=='n') & (allstim.Condition[i]=='New'): 
                        accuracy = 1 
                    elif (thisKey[0]=='n') & (allstim.Condition[i]=='Lure'): 
                        accuracy = 1  
                    else: 
                        accuracy = 0
                elif thisKey[0] in ['q', 'escape']:
                    win.close()
                    core.quit() #abort experiment
        else:
            answer = 999
            accuracy = 999
            rt = 999
        event.clearEvents()
    # add response to output
    f5.write('\n %s, %s, %s, %s, %i, %f' %(fileName, allstim.Condition[i], word, answer, accuracy, rt))
    #buffer
    core.wait(0.1)

# data output
f5.close()

#give some on screen feedback

feedback1.draw()
goodbye = visual.TextStim(mywin, pos=[0,-3], text='Thank you for participating! Please let your experimenter know that you are ready for the next section.')
goodbye.draw()
mywin.flip()
event.waitKeys() #wait for participant to respond

# clean up
mywin.close()
core.quit()
