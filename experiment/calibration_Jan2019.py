#!/usr/bin/env python2
# -*- coding: utf-8 -*-

"""calibrate participants contrast settings to achieve 90% accuracy using a staircase method"""
from __future__ import print_function

from psychopy import core, visual, gui, data, event, logging
from psychopy.tools.filetools import fromFile, toFile
import time, numpy, random, os


# setting default parameters
expInfo = {'participant':'aa', 'contrast':.2}
expInfo['dateStr']= data.getDateStr() #add the current time
#present a dialogue to change params
dlg = gui.DlgFromDict(expInfo, title='calibration', fixed=['dateStr'])
if dlg.OK:
    if not os.path.exists(expInfo['participant']):
        os.makedirs(expInfo['participant'])
    toFile(expInfo['participant'] + '/lastParams.pickle', expInfo)#save params to file for next time
else:
    core.quit()#the user hit cancel so exit

#make a text file to save data

fileName = expInfo['participant'] + '/' + expInfo['participant'] + '_cal_' + expInfo['dateStr']
dataFile = open(fileName+'.csv', 'w')#a simple text file with 'comma-separated-values'
dataFile.write('orientation,contrast,correct,RT,targetOn,\n')

#create the staircase handler
# Not confident about input parameters -- want to use this to find contrast for 90% correct
staircase = data.QuestHandler(expInfo['contrast'], expInfo['contrast']/1.5,
    pThreshold=0.9, gamma=0.01,
    nTrials=50, minVal=0, maxVal=1)

staircaseP = data.QuestHandler(expInfo['contrast'], expInfo['contrast']/1.5,
    pThreshold=0.9, gamma=0.01,
    nTrials=8, minVal=0, maxVal=1)

#create window and stimuli

# KD needs to change -- this is for testing purposes but need to make it full screen for patients
win = visual.Window([1920,1080],allowGUI=False, fullscr=True, monitor='testMonitor', units='deg')
win.recordFrameIntervals = True
win.refreshThreshold = 1/60 + 0.004
logging.console.setLevel(logging.WARNING)

# orientation of stimulus
lOri=330.0
rOri=30.0
target = visual.GratingStim(win, sf=1, size=4, mask='gauss', ori=lOri, contrast=expInfo['contrast'], pos=(0.0,0.0))
fixation = visual.GratingStim(win, color=-1, colorSpace='rgb', tex=None, mask='circle', size=0.2)

#################################################################################
########################### instructions ########################################
#################################################################################
 
inst_path='Cal_Instructions/Slide'

for i in numpy.arange(1,9):
    inst = visual.SimpleImageStim(win, image=inst_path+str(i)+'.png', pos=[0,0])#,units='norm', size=[2,2])
    inst.draw()
    win.flip()
    core.wait(1)
    event.waitKeys()


#display instructions and wait
message1 = visual.TextStim(win, pos=[0,+3],text='Press the left or right arrow keys to identify the tilt direction.')
message2 = visual.TextStim(win, pos=[0,-3],
    text="Hit any key when ready.")
ASAP = visual.TextStim(win,text='Please respond quickly')
message1.draw()
message2.draw()
fixation.draw()
win.flip()#to show our newly drawn 'stimuli'
#pause until there's a keypress
event.waitKeys()

#################################################################################
########################### Practice ############################################
#################################################################################

expClock = core.Clock()
for thisContrast in staircaseP: #will step through the staircase
        
    # set contrast
    target.setContrast(thisContrast)
    
    #draw mask during ISI
    fixation.draw()
    win.flip()
    
    # KD my timing is terrible!!!!  The best way to do timing is with frames
    core.wait(1) # in seconds
    
    # set orientation of target
    orient= random.choice([lOri,rOri]) #will be either +1(right) or -1(left)
    target.setOri(orient) #in other location
    
    #draw target
    target.setAutoDraw(True)  # I used autodraw but I can't remember why
    win.flip()  #  can you set clocks during the flip?
    trialClock = core.Clock()
    targetOn=expClock.getTime()
    event.clearEvents()
    thisResp=None
    allKeys=None
    continueRoutine=1
    while (continueRoutine==1):
        win.flip()
        allKeys=event.getKeys(keyList=["left","right",'q'],timeStamped=trialClock)

        if trialClock.getTime()>3: # remind if take too long to respond
            ASAP.setAutoDraw(True)
            fixation.setAutoDraw(False)
        if trialClock.getTime()>.5: # remove target and replace with fixation
            target.setAutoDraw(False)
            
        if len(allKeys)>0:
            continueRoutine=0
            
    target.setAutoDraw(False)
    fixation.setAutoDraw(True)
    ASAP.setAutoDraw(False)
    
    if len(allKeys)>0:
        for thisKey in allKeys:
            rt=thisKey[1]
            if thisKey[0]=='left':
                if orient==lOri: thisResp = 1 #correct
                else: thisResp = 0             #incorrect
            elif thisKey[0]=='right':
                if orient==rOri: thisResp = 1 #correct
                else: thisResp = 0             #incorrect
            elif thisKey[0] in ['q']:
                win.close()
                core.quit() #abort experiment
    event.clearEvents() #must clear other (eg mouse) events - they clog the buffer

    #add the data to the staircase so it can calculate the next level
    staircaseP.addResponse(thisResp)


message3 = visual.TextStim(win, pos=[0,+3],text='Great Work!')
message4 = visual.TextStim(win, pos=[0,-3],
    text="Do you have any questions before we start?")
message3.draw()
message4.draw()
win.flip()
core.wait(1)
event.waitKeys()

message1.draw()
message2.draw()
fixation.draw()

win.flip()#to show our newly drawn 'stimuli'
event.waitKeys()
expClock = core.Clock()

#################################################################################
########################### Experiment ##########################################
#################################################################################

for thisContrast in staircase: #will step through the staircase
    
    
    # set contrast
    target.setContrast(thisContrast)
    
    #draw mask during ISI
    fixation.setAutoDraw(True)
    win.flip()
    
    # KD my timing is terrible!!!!  The best way to do timing is with frames
    core.wait(1) # in seconds
    
    # set orientation of target
    orient= random.choice([lOri,rOri]) #will be either +1(right) or -1(left)
    target.setOri(orient) #in other location
    
    #draw target
    target.setAutoDraw(True)  
    win.flip() 
    targetOn=expClock.getTime()
    trialClock = core.Clock()
    
    #clear responses and then check for new responses during stimulus presentaiton
    event.clearEvents()
    thisResp=None
    allKeys=None
    continueRoutine=1
    while (continueRoutine==1):
        win.flip()
        allKeys=event.getKeys(keyList=["left","right",'q'],timeStamped=trialClock)

        if trialClock.getTime()>3: # remind if take too long to respond
            ASAP.setAutoDraw(True)
            fixation.setAutoDraw(False)
        if trialClock.getTime()>.5: # remove target and replace with fixation
            target.setAutoDraw(False)
            
        if len(allKeys)>0:
            continueRoutine=0
            
    target.setAutoDraw(False)
    fixation.setAutoDraw(True)
    ASAP.setAutoDraw(False)
    
    if len(allKeys)>0:
        for thisKey in allKeys:
            rt=thisKey[1]
            if thisKey[0]=='left':
                if orient==lOri: thisResp = 1 #correct
                else: thisResp = 0             #incorrect
            elif thisKey[0]=='right':
                if orient==rOri: thisResp = 1 #correct
                else: thisResp = 0             #incorrect
            elif thisKey[0] in ['q']:
                win.close()
                core.quit() #abort experiment
    event.clearEvents() #must clear other (eg mouse) events - they clog the buffer

    #add the data to the staircase so it can calculate the next level
    staircase.addResponse(thisResp)
    dataFile.write('%.1f,%.3f,%i,%.5f,%.2f\n' %(target.ori, thisContrast, thisResp, rt, targetOn))

#staircase has ended
dataFile.close()
staircase.saveAsPickle(expInfo['participant'] + '/' + expInfo['participant'] + '_cal_' + expInfo['dateStr'] + '.p') #special python binary file to save all the info

#give some on screen feedback
feedback1 = visual.TextStim(win, pos=[0,+3],
    text='final mean contrast was = %.3f' %staircase.mean())  
feedback2 = visual.TextStim(win, pos=[0,-3],
    text='final mean accuracy was = %.3f' %numpy.mean(staircase.data[-11:-1]))
feedback1.draw()
feedback2.draw()
fixation.draw()
win.flip()
event.waitKeys() #wait for participant to respond

print('Overall, %i frames were dropped.' % win.nDroppedFrames)
win.close()
core.quit()
