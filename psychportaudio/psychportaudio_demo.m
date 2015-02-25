%This script plays a sample tone repeatedly. To exit the experiment, press
%escape.

clear all;

try

%% Set up input devices

    numDevices=PsychHID('NumDevices');
    devices=PsychHID('Devices');

    for n=1:numDevices
        if (findstr(devices(n).transport, 'USB') & findstr(devices(n).usageName,'Keyboard'))
            inputDevice=[n n];
            break
        elseif (findstr(devices(n).transport,'Bluetooth') & findstr(devices(n).usageName, 'Keyboard'))
            inputDevice=[n n];
            break
        elseif (findstr(devices(n).transport,'ADB') & findstr(devices(n).usageName,'Keyboard'))
            inputDevice=[n n];
            break
        end
    end
    if ~exist('inputDevice')
        error('Can not find input device!');
    end
    fprintf('Using Device #%d (%s)\n',inputDevice(2),devices(n).product);

%% Load stimuli

    load soundfile.mat;

%% Set up sound

    wave=y;
    freq=Fy*.5;
    nrchannels = size(wave,1);

    % Default to auto-selected default output device:
    deviceid = -1;

    % Request latency mode 2, which used to be the best one in our measurement:
    reqlatencyclass = 2; % class 2 empirically the best, 3 & 4 == 2

    % Initialize driver, request low-latency preinit:
    InitializePsychSound(1);

    % Open audio device for low-latency output:
    pahandle = PsychPortAudio('Open', deviceid, [], reqlatencyclass, freq, nrchannels);


%% Experiment: play tones

    fprintf('\n\nThis script plays a sample tone repeatedly.\n');
    fprintf('To exit the experiment, press escape.\n\n');

    while 1
        %Play tone
        PsychPortAudio('FillBuffer', pahandle, wave);
        PsychPortAudio('Start', pahandle, 1, 0, 0);

        %Play for 1s
        start_time=GetSecs;
        while GetSecs<start_time+1
            %%% check for escape key %%%
            [keyIsDown,secs,keyCode]=KbCheck(inputDevice(2));
            escapekey = KbName('escape');
            if keyIsDown
                if keyCode(escapekey)
                    error('User escaped experiment!')
                end
            end
        end

        % Stop playback:
        PsychPortAudio('Stop', pahandle);

        %Silence for 1s
        start_time=GetSecs;
        while GetSecs<start_time+1
            %%% check for escape key %%%
            [keyIsDown,secs,keyCode]=KbCheck(inputDevice(2));
            escapekey = KbName('escape');
            if keyIsDown
                if keyCode(escapekey)
                    error('User escaped experiment!')
                end
            end
        end

    end

%% Finish experiment

catch
    % Close the audio device:
    PsychPortAudio('Close', pahandle);
    
    rethrow(lasterror)
end

return
