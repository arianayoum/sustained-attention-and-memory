#!/usr/bin/env python2
# -*- coding: utf-8 -*-

"""Run SAT Using Customized Parameters"""


from __future__ import print_function

from psychopy import core, visual, gui, data, event, logging
from psychopy.tools.filetools import fromFile, toFile
import time, numpy, random, os

subgui = gui.Dlg()
subgui.addField("Subject ID:")
subgui.addField("Stimulation Number:")

subgui.show()
subj_id = subgui.data[0]
stim_sess = subgui.data[1]

files = gui.fileOpenDlg(tryFilePath='./'+subj_id, tryFileName='calibration*',prompt='Select Most Recent Calibrarion File', allowed='*p.psydat')

calib = fromFile(files[0])
Contrast=calib.mean()


#make a text file to save data

fileName = subj_id + '/' + subj_id + '_stim_' + stim_sess + '_' + data.getDateStr() + '_20vs80'
dataFile = open(fileName+'.csv', 'w')#a simple text file with 'comma-separated-values'
dataFile.write('subject, trial, orientation, contrast, correct, RT, targetOn, ISI_dur\n')

win = visual.Window([1920,1080],allowGUI=False, fullscr=True, monitor='testMonitor', units='deg')
win.recordFrameIntervals = True
win.refreshThreshold = 1/60 + 0.004
logging.console.setLevel(logging.WARNING)

#create window and stimuli
lOri=330.0
rOri=30.0
target = visual.GratingStim(win, sf=1, size=4, mask='gauss', ori=lOri, contrast=Contrast*2, pos=(0.0,0.0))
fixation = visual.GratingStim(win, color=-1, colorSpace='rgb', tex=None, mask='circle', size=0.2)

#timing
ntr = 400  # number of trials -- should be 300s/trial_length
trial_length=.2+2
exp_length = ntr*trial_length + 2 #in seconds

on_times = numpy.arange(2, exp_length, trial_length)
        
        
###### KD need to add better instructions and a practice session
# instructions - make a loop that presents images (.tiff) full screen (keep each on until key is pressed) (would be super cool if it could go backwards)

# practice - make a mini versions of the experiment with no output, just 8 trials, doesn't need staircase (would be best if it's optional)



#display instructions and wait
inst_path='SAT2_Instructions/Slide'
 
for i in numpy.arange(1,2):
    inst = visual.SimpleImageStim(win, image=inst_path+str(i)+'.png', pos=[0,0])#,units='norm', size=[2,2])
    inst.draw()
    win.flip()
    core.wait(1)
    event.waitKeys()
    
message1 = visual.TextStim(win, pos=[0,+3],text='Hit any key when ready.')
message2 = visual.TextStim(win, pos=[0,-3],
    text="Then press the left or right arrows to identify the tilt direction.")
message1.draw()
message2.draw()
fixation.draw()
win.flip()#to show our newly drawn 'stimuli'
#pause until there's a keypress
event.waitKeys()
expClock = core.Clock()

for tr in range(ntr): #will step through the staircase
    
    #blank screen during ISI
    win.flip()
    
    # set orientation of target
    orient= numpy.random.choice([lOri,rOri], 1,p=[0.20, 0.80]) #will be either +1(right) or -1(left)
    target.setOri(orient[0]) #in other location
    
    #draw target
    ISI_dur = on_times[tr] - expClock.getTime()
    core.wait(ISI_dur)
    
    target.setAutoDraw(True)
    win.flip()
    trialClock = core.Clock()
    targetOn=expClock.getTime()
    core.wait(.2)

    #draw fixation
    target.setAutoDraw(False)
    win.flip()
    
    #get response   

    thisResp=9999
    rt=9999
    allKeys=event.waitKeys(maxWait=2, keyList=["left","right",'q'],timeStamped=trialClock)
    if allKeys!= None:
        for thisKey in allKeys:
            rt=thisKey[1]
            if thisKey[0]=='left':
                if orient==lOri: thisResp = 1#correct
                else: thisResp = 0             #incorrect
            elif thisKey[0]=='right':
                if orient==rOri: thisResp = 1#correct
                else: thisResp = 0             #incorrect
            elif thisKey[0] in ['q', 'escape']:
                win.close()
                core.quit() #abort experiment
    event.clearEvents() #must clear other (eg mouse) events - they clog the buffer

    dataFile.write('%s, %i, %.1f,%.3f,%i,%.5f,%.2f,%.2f\n' %(subj_id, tr+1, target.ori, Contrast, thisResp, rt, targetOn, ISI_dur))

#experiment has ended
dataFile.close()

#give some on screen feedback

feedback1 = visual.TextStim(win, pos=[0,0],
    text='Great Work! Please call your experimenter.')
feedback1.draw()
win.flip()
event.waitKeys() #wait for participant to respond

print('Overall, %i frames were dropped.' % win.nDroppedFrames)
win.close()
core.quit()
