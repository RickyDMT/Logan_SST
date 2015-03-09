%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%% Stopfmri %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Adam Aron 12-01-2005
%%% Adapted for OSX Psychtoolbox by Jessica Cohen 12/2005
%%% Modified for use with new BMC trigger-same device as button box by JC 1/07
%%% Sound updated and modified for Jess' dissertation by JC 10/08

commandwindow;

clear all;
% output version
[studyFolder,~,~] = fileparts(which('SSRT.m'));
script_name='Stopfmri: optimized SSD tracker for fMRI';
script_version='2';
revision_date='02-24-15'; 
% Updated by LEK to incorporate the following changes:
% Output and log files go to output folder
% Includes colors for slowing (threshes set below)
% Can turn off the colors by setting colorFlags=0
% need to set "studyFolder" to be the appropriate path (the folder with
% output folder inside)

% BEFORE RUNNING THIS SCRIPT:
% Make sure folder with this script, the ladder files (e.g. st1b1.mat), and
% soundfile.mat (the beep) are in the path

% Ver2: updated to not need ladder files. 
% -- Reworked to look like Logan, 1997.


notes={'Design developed by Aron, Newman and Poldrack, based on Aron et al. 2003'};

highThresh = .750;
lowThresh = .500;
colorFlags=0;
red=[255,0,0,255];  
orange=[255,128,0,255];
    
% read in subject initials
fprintf('%s %s (revised %s)\n',script_name,script_version, revision_date);

prompt={'SUBJECT ID' 'Session'};
defAns={'4444' '1'};

answer=inputdlg(prompt,'Please input subject info',1,defAns);

subject_code=str2double(answer{1});
sub_session = str2double(answer{2});

% subject_code=input('Enter subject number (integer only): ');
% sub_session=input('Is this the subject''s first or second session? (Enter 1 or 2): ');
%scannum_temp=input('Enter scan number: ');
%scannum_temp=sub_session;
%scannum=scannum_temp;

%MRI=input('Are you scanning? 1 if yes, 0 if no: ');
MRI = 0; % Use the above line instead if you'd like to have the option to wait for a trigger pulse

% if sub_session==1,
    LADDER1IN=250; %input('Ladder1 start val (e.g. 250): ');
    LADDER2IN=250; %input('Ladder2 start val (e.g. 350): ');
    %Ladder Starts (in ms):
    Ladder1=LADDER1IN;
    Ladder(1,1)=LADDER1IN;
    Ladder2=LADDER2IN;
    Ladder(2,1)=LADDER2IN;
% elseif sub_session==2, %% this code looks up the last value in each staircase
%     trackfile=input('Enter name of subject''s previous ''mat'' file to open: ','s');
%     load(trackfile);
%     clear Seeker; %gets rid of this so it won't interfere with current Seeker
%     sub_session=2;
%     scannum=sub_session;
%     startval=length(Ladder1);
%     Ladder(1,1)=Ladder1(startval);
%     Ladder(2,1)=Ladder2(startval);
    % elseif scannum>1, %% this code looks up the last value in each staircase
    %     trackfile=input('Enter name of prior #stop_fmri1 to open: ','s');
    %     load(trackfile);
    %     clear Seeker; %gets rid of this so it won't interfere with current Seeker
    %     scannum=scannum_temp; %because first file has scannum saved as 1, overwrite that for inputted scan number
    %     startval=length(Ladder1);
    % 	Ladder(1,1)=Ladder1(startval);
    % 	Ladder(2,1)=Ladder2(startval);
% end;


%load relevant input file for scan (there MUST be st1b1.mat & st1b2.mat)
% inputfile=sprintf('st%db%d.mat',subject_code,sub_session);
% load(inputfile); %variable is trialcode

% write trial-by-trial data to a text logfile
d=clock;
logfile=sprintf('sub%d_scan%d_stopsig.log',subject_code,sub_session);
logfile = [studyFolder(1:end-7) 'output' filesep logfile];  %removes "scripts" from directory path, adds "output" to save results there
fprintf('A log of this session will be saved to %s\n',logfile);
fid=fopen(logfile,'a');
if fid<1,
    error('could not open logfile!');
end;

fprintf(fid,'Started: %s %2.0f:%02.0f\n',date,d(4),d(5));
WaitSecs(1);

%Seed random number generator
rand('state',subject_code);




try  % goes with catch at end of script 
    %% set up input devices
%     numDevices=PsychHID('NumDevices');
%     devices=PsychHID('Devices');
    inputDevice = [];
%     if MRI==1,
%         for n=1:numDevices,
%             if (findstr(devices(n).transport,'USB') & findstr(devices(n).usageName,'Keyboard') & (devices(n).productID==612 | devices(n).vendorID==1523 | devices(n).totalElements==244)),
%                 inputDevice=n;
%                 %else,
%                 %    inputDevice=6; % my keyboard
%             end;
%             if (findstr(devices(n).transport,'USB') & findstr(devices(n).usageName,'Keyboard') & (devices(n).productID==566 | devices(n).vendorID==1452)),
%                 controlDevice=n;
%             end
%         end;
%         fprintf('Using Device #%d (%s)\n',inputDevice,devices(inputDevice).product);
%     else,
%         for n=1:numDevices,
%             if (findstr(devices(n).transport,'USB') & findstr(devices(n).usageName,'Keyboard')),
%                 inputDevice=[n n];
%                 break,
%             elseif (findstr(devices(n).transport,'Bluetooth') & findstr(devices(n).usageName,'Keyboard')),
%                 inputDevice=[n n];
%                 break,
%             elseif findstr(devices(n).transport,'ADB') & findstr(devices(n).usageName,'Keyboard'),
%                 inputDevice=[n n];
%             end;
%         end
%         if isempty(inputDevice)
%             inputDevice = [2 2];
%             n = 2;
%         end
%         fprintf('Using Device #%d (%s)\n',inputDevice,devices(n).product);
%     end;
    
    % set up screens
        % set up screens
        Screen('Preference', 'SkipSyncTests', 1);
    fprintf('setting up screen\n');
    screens=Screen('Screens');
    screenNumber= max(Screen('Screens'));
    [w, ~] =Screen('OpenWindow', screenNumber,0,[],32,2);
    [wWidth, wHeight]=Screen('WindowSize', w);
%     grayLevel=120;
%     Screen('FillRect', w, grayLevel);
    Screen('Flip', w);

    black=BlackIndex(w); % Should equal 0.
    white=WhiteIndex(w); % Should equal 255.

    xcenter=wWidth/2;
    ycenter=wHeight/2;

    theFont='Arial';
    Screen('TextSize',w,36);
    Screen('TextFont',w,theFont);
    Screen('TextColor',w,white);
   
    CircleSize=400;
    CirclePosX=xcenter-92;
    CirclePosY=ycenter-250;
    ArrowSize=150;
    ArrowPosX=xcenter-25;
    ArrowPosY=ycenter-125;

    HideCursor;

    %Adaptable Constants
    % "chunks", will always be size 64:
    NUMCHUNKS=4;  %gngscan has 4 blocks of 64 (2 scans with 2 blocks of 64 each--but says 128 b/c of interspersed null events)
    %StepSize = 50 ms;
    Step=50;
    %Interstimulus interval (trial time-.OCI) = 2.5s
    ISI=1.5; %set at 1.5 
    %BlankScreen Interval is 1.0s
    BSI=1 ;  %NB, see figure in GNG4manual (set at 1 for scan)
    %Only Circle Interval is 0.5s AKA this is fixation?
    OCI=0.5;
    arrow_duration=1; %because stim duration is 1.5 secs in opt_stop

    %%% FEEDBACK VARIABLES
    if MRI==1,
        %trigger = KbName('t');
        trigger = [52];
        blue = KbName('b');
        yellow = KbName('y');
        green = KbName('g');
        red = KbName('r');
        %LEFT=[98 5 10];   %blue (5) green (10)
        LEFT = [91];
        RIGHT=[94];
        %RIGHT=[121 28 21]; %yellow (28) red (21)
    else,
        LEFT=[54];  %<
        RIGHT=[55]; %>
    end;
    
    if sub_session==1;
        error=zeros(1, NUMCHUNKS/2);
        rt = zeros(1, NUMCHUNKS/2);
        count_rt = zeros(1, NUMCHUNKS/2);
    end;

    
    
    
    
    %%%% Setting up the sound stuff
    %%%% Psychportaudio
    load soundfile.mat %%% NEED SOMETHING PERSONALIZED TO ME????? I.E. IF WANT THE SOUND HIGHER??
    %wave=y;
    wave=sin(1:0.25:1000);
    %freq=Fy*1.5; % change this to change freq of tone
    freq=22254;
    nrchannels = size(wave,1);
    % Default to auto-selected default output device:
    deviceid = -1;
    % Request latency mode 2, which used to be the best one in our measurement:
    reqlatencyclass = 2; % class 2 empirically the best, 3 & 4 == 2
    % Initialize driver, request low-latency preinit:
    InitializePsychSound(1);
    % Open audio device for low-latency output:
    pahandle = PsychPortAudio('Open', deviceid, [], reqlatencyclass, freq, nrchannels);
    %Play the sound
    PsychPortAudio('FillBuffer', pahandle, wave);
    PsychPortAudio('Start', pahandle, 1, 0, 0);
    WaitSecs(1);
    PsychPortAudio('Stop', pahandle);
    %%%% Old way
    %     Snd('Open');
    %     samp = 22254.545454;
    %     aud_stim = sin(1:0.25:1000);
    %     Snd('Play',aud_stim,samp);
    
    
    %%%%%%%%%%%%%% Stimuli and Response on same matrix, pre-determined
    % The first column is trial number;
    % The second column block number;
    % The third column is 0 = Go, 1 = NoGo; 2 is null, 3 is notrial (kluge, see opt_stop.m)
    % The fourth column is 0=left, 1=right arrow; 2 is null
    % The fifth column is ladder number (1-2);
    % The sixth column is the value currently in "LadderX", corresponding to SSD
    % The seventh column is subject response (no response is 0);
    % The eighth column is ladder movement (-1 for down, +1 for up, 0 for N/A)
    % The ninth column is their reaction time (sec)
    % The tenth column is their actual SSD (for error-check)
    % The 11th column is their actual SSD plus time taken to run the command
    % The 12th column is absolute time since beginning of task that trial begins
    % The 13th column is the time elapsed since the beginning of the block at moment when arrows are shown
    % The 14th column is the actual SSD for error check (time from arrow displayed to beep played)
    % The 15th column is the duration of the trial from trialcode
    % The 16th column is the time_course from trialcode
    
    
    %this puts trialcode into Seeker
    % trialcode was generated in opt_stop and is balanced for 4 staircase types every 16 trials, and arrow direction
    %  see opt_stop.m in /gng/optmize/stopping/
    % because of interdigitated null and true trial, there will thus be four staircases per 32 trials in trialcode
    
%     Produce a trial code that makes any goddamned sense
totes_trials = 512;
blocks = 4;
trials = 128;
trialcode = NaN(totes_trials,4);

% Generate basic setup for a given block...
trial_gen = [ones(fix(trials/4),1); zeros((trials-fix(trials/4)),1)];    %Is it go or nogo?
trial_gen = [trial_gen repmat([0;1],trials/2,1)];                       %Is is Left or right; 0=left, 1=right arrow
trial_gen = [trial_gen [repmat([1;2],fix(trials/8),1); zeros((trials-fix(trials/4)),1)]];     %is ladder number (1-2);



for bbb = 1:blocks
    block_start = ((bbb-1)*trials)+1;
    trial_gen = trial_gen(randperm(length(trial_gen)),:);   %Always shuffling shuffling: Shuffle the trial generator
    
    trialcode(block_start:block_start+trials-1,1) = trial_gen(:,1);   %Is it a go or nogo trial? 0 = Go, 1 = NoGo;
    trialcode(block_start:block_start+trials-1,2) = trial_gen(:,2);
    trialcode(block_start:block_start+trials-1,3) = trial_gen(:,3);
    trialcode(block_start:block_start+trials-1,4) = bbb;

    
end
    
    
    
    for  tc=1:totes_trials                         %go/nogo        arrow dir       staircase    initial staircase value                    duration       timecourse
        if trialcode(tc,3)>0,
            Seeker(tc,:) = [tc trialcode(tc,4)  trialcode(tc,1) trialcode(tc,2) trialcode(tc,3) Ladder(trialcode(tc,3)) 0 0 0 0 0 0 0 0 0 0];
        else
            Seeker(tc,:) = [tc sub_session trialcode(tc,1) trialcode(tc,2) trialcode(tc,3) 0 0 0 0 0 0 0 0 0 0 0];
        end;
    end;
    
    
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %%%%%%%%%%%%%%%%%%%%%%%%%% TRIAL PRESENTATION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
%     startstring = sprintf('Get ready for scan number %d!',sub_session);
%     Screen('DrawText',w,startstring,100,100);
%     if MRI==1
%         Screen('DrawText',w,'Waiting for trigger...',xcenter-150,ycenter);
%         Screen('Flip',w);
%     else
%NEED TO UPDATE THESE INSTRUCTIONS
        DrawFormattedText(w,'In this task, you will see the letter "X" or "O."\nPress "/" when you see an X and "z" when you see an O.\n\n Be as quick as you can without making errors.\n\nPress any key to continue.','center','center',white,70,[],[],1.5);
        Screen('Flip',w);
        KbWait([],2);
        
        DrawFormattedText(w,'Occasionally, the X or O will be followed by a beep. Do not respond on that trial if you hear a beep. Inhibit your response but don''t worry if you are unable to inhibit your response.\n\nThe beeps will occur at different times, so sometimes you will be able to stop and sometimes you will not be able to.\n\n Press any key to continue','center','center',white,70,[],[],1.5);
        Screen('Flip',w);
        KbWait([],2);
        
        DrawFormattedText(w,'Stopping and going are equally important. Do not let the "stop task" (the beeps) interfere with the "go task" (the letters).\n\n Press any key to continue.','center','center',white,70,[],[],1.5);
        Screen('Flip',w);
        KbWait([],2);
        
        DrawFormattedText(w,'If you have any questions, please ask the assessor now!','center','center',white,60,[],[],1.5);
        Screen('Flip',w);
        WaitSecs(5);
        
        DrawFormattedText(w,'Press any key when you are ready to begin the task.','center','center',white,60,[],[],1.5);
        Screen('Flip',w);
        KbWait([],2);
%         Screen('DrawText',w,'Press the left button (z) if you see O',100,175);
%         Screen('DrawText',w,'Press the right button (/) if you see X',100,225);
%         Screen('DrawText',w,'Press the button as FAST as you can',100,300);
%         Screen('DrawText',w,'when you see the arrow.',100,350);
%         Screen('DrawText',w,'But if you hear a beep, try very hard',100,425);
%         Screen('DrawText',w,'to STOP yourself from pressing the button.',100,475);
%         Screen('DrawText',w,'Stopping and Going are equally important.',100,550);
%         Screen('DrawText',w,'Press any key to go on.',100,625);
        Screen('Flip',w);
%     sca
%  end;
    
    if MRI==1,
        secs=KbTriggerWait(trigger,inputDevice);
        %secs = KbTriggerWait(KbName('t'),controlDevice);
    else % If using the keyboard, allow any key as input
        noresp=1;
        while noresp,
            [keyIsDown,secs,keyCode] = KbCheck(inputDevice);
            if keyIsDown & noresp,
                noresp=0;
            end;
        end;
        WaitSecs(0.001);
    end;
    % WaitSecs(0.5);  % prevent key spillover--ONLY FOR BEHAV VERSION
    
    if MRI==1
        DisableKeysForKbCheck(trigger); % So trigger is no longer detected
    end
    
    anchor=GetSecs;
    Pos=1;
    
    FLAG_FASTER=0;
    
    for block=1:blocks %2	  %because of way it's designed, there are two blocks for every scan
        
        for a=1:16 %8     %  now we have 16 chunks of 8 trials (but they used to use 16 because of the null interspersed trials)
            %for a=1:1,  % short for troubleshooting
            for b=1:8   %  (but they used to use 16 because of the null interspersed trials)
                
%                 if FLAG_FASTER==2
%                     circleColor = red;  %red [255,0,0,255]
%                     Screen('TextColor',w,red);
%                 elseif FLAG_FASTER==1
%                     circleColor = orange; %orange [255,128,0,255]
%                     Screen('TextColor',w,orange);
%                 else
%                     circleColor = white;
%                     Screen('TextColor',w,white);
%                 end
                
                if Seeker(Pos,3)~=2, %% ie this is not a NULL event
                    %This part displays fixation...
                    Screen('TextSize',w,ArrowSize);
                    Screen('TextFont',w,'Courier');
                    DrawFormattedText(w,'+','center','center',[255 255 255]);
%                     Screen('TextSize',w,ArrowSize);
                    Screen('TextFont',w,'Arial');

%                   %I think this tries to align the timer iwth
%                   preconceived timings to match fMRI?
%                     while GetSecs - anchor < Seeker(Pos,16),
%                     end; %waits to synch beginning of trial with 'true' start
                    
                    trial_start_time = Screen('Flip',w);
%                     trial_start_time = GetSecs;
                    Seeker(Pos,12)=trial_start_time-anchor; %absolute time since beginning of task
                    WaitSecs(OCI);
                end;
                
                if Seeker(Pos,3)~=2 %% ie this is not a NULL event

                    %This part displays Xs or Os

                    if Seeker(Pos,4)==0
                        DrawFormattedText(w,'O','center','center',[255 255 255]);
                    else
                        DrawFormattedText(w,'X','center','center',[255 255 255]);
                    end;
                    noresp=1;
                    notone=1;
                    arrow_start_time = Screen('Flip',w);
%                     arrow_start_time = GetSecs;
                    
                    
                    while (GetSecs-arrow_start_time < arrow_duration & noresp),
                        [keyIsDown,secs,keyCode] = KbCheck(inputDevice);
                        if MRI==1
                            if keyIsDown && noresp
                                tmp=KbName(keyCode);
                                Seeker(Pos,7)=KbName(tmp(1));
                                Seeker(Pos,9)=GetSecs-arrow_start_time;
                                noresp=0;
                            end;
                        else
                            if keyIsDown && noresp
                                try
                                    tmp=KbName(keyCode);
                                    if iscell(tmp) %&& any(KbName(tmp) == KbName('z')) || any(KbName(tmp) == KbName('/?'))
                                        %Looks like multiple buttons were
                                        %smashed...just choose one or 999?
                                        if any(KbName(tmp) == KbName('z'))
                                            Seeker(Pos,7) = KbName('z');
                                        elseif any(KbName(tmp) == KbName('/?'))
                                            Seeker(Pos,7) = KbName('/?');
                                        end
%                                     if length(tmp) > 1 && (tmp(1)=='z' || tmp(1)=='/?')
%                                         Seeker(Pos,7)=KbName(tmp(2));
                                    elseif ~iscell(tmp)
                                        Seeker(Pos,7)=KbName(tmp);
                                    else
                                        Seeker(Pos,7)=9999;
                                    end
                                catch
                                    Seeker(Pos,7)=9999;
                                end;
                                if b==1 && GetSecs-arrow_start_time<0,
                                    Seeker(Pos,9)=0;
                                    Seeker(Pos,13)=0;
                                else
                                    Seeker(Pos,9)=GetSecs-arrow_start_time; % RT
                                end;
                                noresp=0;
                            end;
                        end;
                        WaitSecs(0.001);
                        if Seeker(Pos,3)==1 && GetSecs - arrow_start_time >=Seeker(Pos,6)/1000 && notone,
                            %% Psychportaudio
                            PsychPortAudio('FillBuffer', pahandle, wave);
                            PsychPortAudio('Start', pahandle, 1, 0, 0);
                            Seeker(Pos,14)=GetSecs-arrow_start_time;
                            notone=0;
                            %WaitSecs(1); % So sound plays for set amount of time; if .05, plays twice, otherwise doen't really make it last longer
                            %PsychPortAudio('Stop', pahandle);
                            % Try loop to end sound after 1 sec, while
                            % still looking for responses-DOESN"T WORK!!!!!
                            while GetSecs<Seeker(Pos,14)+1,
                                %%% check for escape key %%%
                                [keyIsDown,secs,keyCode] = KbCheck(inputDevice);
                                escapekey = KbName('escape');
                                if keyIsDown & noresp
                                    try
                                        tmp=KbName(keyCode);
                                        if length(tmp) > 1 && (tmp(1)=='z' || tmp(1)=='/?'),
                                            Seeker(Pos,7)=KbName(tmp(2));
                                        else
                                            Seeker(Pos,7)=KbName(tmp(1));
                                        end;
                                    catch
                                        Seeker(Pos,7)=9999;
                                    end;
                                    if b==1 & GetSecs-arrow_start_time<0,
                                        Seeker(Pos,9)=0;
                                        Seeker(Pos,13)=0;
                                    else
                                        Seeker(Pos,9)=GetSecs-arrow_start_time; % RT
                                    end;
                                    noresp=0;
                                end;
                            end;
                            %PsychPortAudio('Stop', pahandle);
                            %% Old way to play sound
                            %Snd('Play',aud_stim,samp);
                            %Seeker(Pos,14)=GetSecs-arrow_start_time;
                            %notone=0;
                        end;

                    end; %end while
                    PsychPortAudio('Stop', pahandle); % If do this,
                    % response doesn't end loop
                end; %end non null
                
                Screen('Flip',w);
                
%                 while(GetSecs - anchor < Seeker(Pos,16) + Seeker(Pos,15)),
%                 end;
                WaitSecs(BSI);
                
                % print trial info to log file
                tmpTime=GetSecs;
                try
                    fprintf(fid,'%d\t%d\t%d\t%d\t%d\t%d\t%d\t%d\t%0.3f\t%0.3f\t%0.3f\t%0.3f\t%0.3f\t%0.3f\t%0.3f\t%0.3f\n',...
                        Seeker(Pos,1:16));
                catch   % if sub responds weirdly, trying to print the resp crashes the log file...instead print "ERR"
                    fprintf(fid,'ERROR SAVING THIS TRIAL\n');
                end;
                
                if colorFlags==1
                    trialRT=Seeker(Pos,9);
                    if Seeker(Pos,3)~=2
                        if trialRT > highThresh
                            FLAG_FASTER = 2;
                        elseif trialRT > lowThresh
                            FLAG_FASTER = 1;
                        else
                            FLAG_FASTER = 0;
                        end
                    end
                end
                
                
                Pos=Pos+1;
                FlushEvents();
                
            end; % end of trial loop
            
            % after each 8 trials, this code does the updating of staircases
            %These three loops update each of the ladders
            for c=(Pos-8):Pos-1,
                %This runs from one to two, one for each of the ladders
                for d=1:2,
                    if (Seeker(c,7)~=0&Seeker(c,5)==d),	%col 7 is sub response
                        if Ladder(d,1)>=Step,
                            Ladder(d,1)=Ladder(d,1)-Step;
                            Ladder(d,2)=-1;
                        elseif Ladder(d,1)>0 & Ladder(d,1)<Step
                            Ladder(d,1)=0;
                            Ladder(d,2)=-1;
                        else
                            Ladder(d,1)=Ladder(d,1);
                            Ladder(d,2)=0;
                        end;
                        
                        if (d==1),
                            [x y]=size(Ladder1);
                            Ladder1(x+1,1)=Ladder(d,1);
                        elseif (d==2),
                            [x y]=size(Ladder2);
                            Ladder2(x+1,1)=Ladder(d,1);
                        end;
                        
                    elseif (Seeker(c,5)==d & Seeker(c,7)==0)
                        Ladder(d,1)=Ladder(d,1)+Step;
                        Ladder(d,2)=1;
                        if (d==1),
                            [x y]=size(Ladder1);
                            Ladder1(x+1,1)=Ladder(d,1);
                        elseif (d==2),
                            [x y]=size(Ladder2);
                            Ladder2(x+1,1)=Ladder(d,1);
                        end
                    end
                end
            end
            
            
            %Updates the time in each of the subsequent stop trials
            for c=Pos:512,
                if (Seeker(c,5)~=0), %i.e. staircase trial
                    Seeker(c,6)=Ladder(Seeker(c,5),1);
                end;
            end;
            %Updates each of the old trials with a +1 or a -1
            for c=(Pos-8):Pos-1,
                if (Seeker(c,5)~=0),
                    Seeker(c,8)=Ladder(Seeker(c,5),2);
                end;
            end;
            
        end; %end of miniblock
        endofblockfont = Screen('TextSize',w,36);
        interblocktext = sprintf('That conludes block %d.\n\nPress any key to continue.',block);
        DrawFormattedText(w,interblocktext,'center','center',[256 256 256]);
        Screen('Flip',w);
        KbWait([],2);
        Screen('TextSize',w,endofblockfont);
    end; %end block loop
    
    
    % Close the audio device:
    PsychPortAudio('Close', pahandle);
    
    
    %try,   %dummy try if need to troubleshoot
    
catch    % (goes with try, line 61)
    rethrow(lasterror);
    
    sca
    ShowCursor;
    
end;


%%%%%%%%%%%%%%% FEEDBACK %%%%%%%%%%%%%%%%
% for t=1:256
%     % go trial   &  left arrow                 respond right   OR  right arrow       respond left
%     if (Seeker(t,3)==0 & ((Seeker(t,4)==0 & sum(Seeker(t,7)==RIGHT)==1)|(Seeker(t,4)==1 & sum(Seeker(t,7)==LEFT)==1))),
%         error(sub_session)=error(sub_session)+1;  % for incorrect responses
%     end;
%     % go trial   &   RT (so respond)  & left arrow            respond left    OR  right arrow       respond right
%     if (Seeker(t,3)==0 & Seeker(t,9)>0 & ((Seeker(t,4)==0 & sum(Seeker(t,7)==LEFT)==1)|(Seeker(t,4)==1 & sum(Seeker(t,7)==RIGHT)==1))),
%         rt(sub_session)=rt(sub_session)+Seeker(t,9);   % cumulative RT
%         count_rt(sub_session)=count_rt(sub_session)+1; %number trials
%     end;
% end;
% 
% Screen('TextSize',w,36);
% Screen('TextFont',w,'Ariel');
% 
% if sub_session==1;
%     Screen('DrawText',w,sprintf('Scanning Block %d',1),100,100);
%     Screen('DrawText',w,sprintf('Mistakes with arrow direction on Go trials: %d', error(1)),100,140);
%     Screen('DrawText',w,sprintf('Correct average RT on Go trials: %.1f (ms)', rt(1)/count_rt(1)*1000),100,180);
%     %     if MRI~=1,
%     %         Screen('DrawText',w,'Press any button to continue',100,220);
%     %     end;
%     Screen('Flip',w);
% end;
% 
% 
% if sub_session==2,
%     Screen('DrawText',w,sprintf('Scanning Block %d',1),100,100);
%     Screen('DrawText',w,sprintf('Mistakes with arrow direction on Go trials: %d', error(1)),100,140);
%     Screen('DrawText',w,sprintf('Correct average RT on Go trials: %.1f (ms)', rt(1)/count_rt(1)*1000),100,180);
%     
%     Screen('DrawText',w,sprintf('Scanning Block %d',2),100,240);
%     Screen('DrawText',w,sprintf('Mistakes with arrow direction on Go trials: %d', error(2)),100,280);
%     Screen('DrawText',w,sprintf('Correct average RT on Go trials: %.1f (ms)', rt(2)/count_rt(2)*1000),100,320);
%     
%     %     if MRI~=1,
%     %         Screen('DrawText',w,'Press any button to continue',100,360);
%     %     end;
%     Screen('Flip',w);
% end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% SAVE DATA %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
d=clock;
outfile=sprintf('%dstop_fmri%d_%s_%02.0f-%02.0f.mat',subject_code,sub_session,date,d(4),d(5));
outfile = [studyFolder(1:end-7) 'output' filesep outfile];
Snd('Close');

params = cell (7,2);
params{1,1}='NUMCHUNKS';
params{1,2}=NUMCHUNKS;
params{2,1}='Ladder1 start';
params{2,2}=Ladder1(1,1);
params{3,1}='Ladder2 start';
params{3,2}=Ladder2(1,1);
params{4,1}='Step';
params{4,2}=Step;
params{5,1}='ISI';
params{5,2}=ISI;
params{6,1}='BSI';
params{6,2}=BSI;
params{7,1}='OCI';
params{7,2}=OCI;

%%% It's better to access these variables via parameters, rather than
%%% saving them...
try
    save(outfile, 'Seeker', 'params', 'Ladder1', 'Ladder2', 'error', 'rt', 'count_rt', 'subject_code', 'sub_session');
catch
    fprintf('couldn''t save %s\n saving to stopsig_fmri.mat\n',outfile);
    save stopsig_fmri;
end;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% if MRI==1,
%     WaitSecs(5);
% else,
%     noresp=1;
%     while noresp,
%         [keyIsDown,secs,keyCode] = KbCheck(inputDevice);
%         if keyIsDown & noresp,
%             noresp=0;
%         end;
%         WaitSecs(0.001);
%     end;
%     WaitSecs(0.5);
% end;

% cWaitSecs(5);

Screen('TextSize',w,36);
Screen('TextFont',w,'Ariel');
DrawFormattedText(w,'That concludes this task. The assessor will be with you shortly.\n\nThank you!','center','center',[250 250 250],80);
Screen('Flip',w);

% if MRI==1,
%     WaitSecs(1);
% else,
%     noresp=1;
%     while noresp,
%         [keyIsDown,secs,keyCode] = KbCheck(inputDevice);
%         if keyIsDown & noresp,
%             noresp=0;
%         end;
%         WaitSecs(0.001);
%     end;
%     WaitSecs(0.5);
% end;

WaitSecs(5);
Screen('Flip',w);
Screen('CloseAll');
ShowCursor;

