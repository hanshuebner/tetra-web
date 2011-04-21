var partial = MochiKit.Base.partial;
var bind = MochiKit.Base.bind;
var map = MochiKit.Base.map;
var log = MochiKit.Logging.log;
var DOM = MochiKit.DOM;
var serializeJSON = MochiKit.Base.serializeJSON;
var evalJSON = MochiKit.Base.evalJSON;

var currentPath = '';
// todo: mvc should be used instead
var controls = {};
var adsrParam = {};                                         // maps from parameter name to handling adsr graph
var socket;
var allPresets = {};
var allLabels = {};

function processSocketMessage (message) {
//    console.log('socket message', message);
    var args = evalJSON(message);
    var command = args.shift();
    switch (command) {
    case 'set':
        var name = args.shift();
        var value = parseInt(args.shift());
        if (controls[name]) {
            try {
                controls[name].setInternalValue(value);
            }
            catch (e) {
                console.log("error can't set control " + name + ' to ' + value + ': ' + e);
            }
        } else {
            console.log('error unknown control', name);
        }
        if (adsrParam[name]) {
            adsrParam[name](name, value);
        }
        break;
    default:
        console.log('error unknown command ' + command);
    }
}

function sendParameterChange(parameter, value)
{
    if (socket) {
        socket.send(serializeJSON(['set', parameter, value]));
    }
}

function tetraToggler(labels, id, title)
{
    var element = document.getElementById(id);
    if (element) {
        $(element).addClass('toggler');
        controls[id] = new Toggler(id, { items: labels || [ title, title ] });
        return controls[id];
    }
}

function tetraSelector(values, id)
{
    var element = document.getElementById(id);
    if (element) {
        $(element).addClass('selector');
        controls[id] = new Selector(id,
                                    { items: values,
                                      fontSize: 10 });
        return controls[id];
    }
}

function tetraSpinner(max,
                      externalMapping,
                      id, title)
{
    var element = document.getElementById(id);
    if (element) {
        $(element).addClass('spinner');
        controls[id] = new Spinner(id,
                                   { title: title,
                                     stateCount: (max + 1),
                                     size: 40,
                                     externalMapping: externalMapping });
        return controls[id];
    }
}

function tetraSpinnerWithRange (min, max)
{
    return partial(tetraSpinner,
                   (max - min),
                   { toDisplay: function (state) {
                       return min + state;
                     },
                     fromDisplay: function (value) {
                         return value - min;
                     }
                   });
};

var tetraControl = {};

var notes = [ "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B" ];

tetraControl.note
    = partial(tetraSpinner,
              120,
              { toDisplay: function (index) {
                  var note = index % 12;
                  var octave = Math.floor(index / 12);
                  return notes[note] + octave;
                }
              });

tetraControl.fineTune
    = partial(tetraSpinner,
              100,
              { toDisplay: function (index) {
                  return -50 + index;
                },
                fromDisplay: function (value) {
                    var index = 50 + parseInt(value);
                    return index;
                }
              });

tetraControl.oscillatorShape
    = partial(tetraSpinner,
              103,
              { toDisplay: function (index) {
                  switch (index) {
                  case 0: return "OFF";
                  case 1: return "SAW";
                  case 2: return "TRI";
                  case 3: return "TRI/SAW";
                  default: return "P" + (index - 4);
                  }
                }
              });

tetraControl.onOff = partial(tetraToggler, null);
tetraControl.glideMode = partial(tetraSelector, [ 'rate', 'rate auto', 'time', 'time auto' ]);
tetraControl.filterPoles = partial(tetraToggler, [ '2 pole', '4 pole' ]);
tetraControl.posNegAmount
    = partial(tetraSpinner,
              254,
              { toDisplay: function (index) {
                  return -127 + index;
                }
              });
tetraControl.lfoFrequency
    = partial(tetraSpinner,
              166,
              { toDisplay: function (index) {
                  if (index < 151) {
                      return index;
                  } else {
                      switch (index) {
                      case 151: return "1/32";
                      case 152: return "1/16";
                      case 153: return "1/8";
                      case 154: return "1/6";
                      case 155: return "1/4";
                      case 156: return "1/3";
                      case 157: return "1/2";
                      case 158: return "1/1.5";
                      case 159: return "1";
                      case 160: return "2/3";
                      case 161: return "2/1";
                      case 162: return "1/3";
                      case 163: return "4/1";
                      case 164: return "6/1";
                      case 165: return "8/1";
                      case 166: return "16/1";
                      }
                  }
                }
              });

tetraControl.lfoShape
    = partial(tetraSelector, ['triangle',
                              'rev saw',
                              'saw',
                              'square',
                              'random' ]);
tetraControl.modulationDestination
    = partial(tetraSelector, ['off',
                              'osc 1 freq',
                              'osc 2 freq',
                              'osc 1+2 freq',
                              'osc mix',
                              'noise level',
                              'osc 1 pulse width',
                              'osc 2 pulse width',
                              'osc 1+2 pulse width',
                              'filter frequency',
                              'resonance',
                              'filter audio mod amt',
                              'vca level',
                              'pan spread',
                              'lfo 1 freq',
                              'lfo 2 freq',
                              'lfo 3 freq',
                              'lfo 4 freq',
                              'all lfo freq',
                              'lfo 1 amt',
                              'lfo 2 amt',
                              'lfo 3 amt',
                              'lfo 3 amt',
                              'all lfo amt',
                              'filter env amt',
                              'amp env amt',
                              'env 3 amt',
                              'all env amounts',
                              'env 1 attack',
                              'env 2 attack',
                              'env 3 attack',
                              'all env attacks',
                              'env 1 decay',
                              'env 2 decay',
                              'env 3 decay',
                              'all env decays',
                              'env 1 release',
                              'env 2 release',
                              'env 3 release',
                              'all env releases',
                              'mod 1 amt',
                              'mod 2 amt',
                              'mod 3 amt',
                              'mod 4 amt',
                              'feedback volume',
                              'sub osc 1 level',
                              'sub osc 2 level',
                              'feedback gain']);
tetraControl.modulationSource
    = partial(tetraSelector, ['off',
                              'sequence track 1',
                              'sequence track 2',
                              'sequence track 3',
                              'sequence track 4',
                              'lfo 1',
                              'lfo 2',
                              'lfo 3',
                              'lfo 4',
                              'filter envelope',
                              'amp envelope',
                              'envelope 3',
                              'pitch bend',
                              'mod wheel',
                              'pressure',
                              'midi breath',
                              'midi foot',
                              'midi expression',
                              'velocity',
                              'note number',
                              'noise']);
tetraControl.timeSignature
    = partial(tetraSelector, ['half note',
                              'quarter note',
                              'eighth note',
                              'eighth note half swing',
                              'eighth note full swing',
                              'eighth note triplets',
                              'sixteenth note',
                              'sixteenth note half swing',
                              'sixteenth note full swing',
                              'sixteenth note triplets',
                              'thirty-second notes',
                              'thirty-second notes triplets',
                              'sixty-fourth note triplets']);
tetraControl.sequencerTrigger
    = partial(tetraSelector, ['normal',
                              'normal, no reset',
                              'no gate',
                              'no gate, no reset',
                              'key step']);
tetraControl.keyMode
    = partial(tetraSelector, ['low note',
                              'low note with re-trigger',
                              'high note',
                              'high note with re-trigger',
                              'last note hit',
                              'last note hit with re-trigger']);
tetraControl.unisonMode
    = partial(tetraSelector, ['1 voice',
                              'all voices',
                              'all voices detune1',
                              'all voices detune2',
                              'all voices detune3']);
tetraControl.arpeggiatorMode
    = partial(tetraSelector, ['up',
                              'down',
                              'up/down',
                              'assign',
                              'random',
                              '2 octaves up',
                              '2 octaves down',
                              '2 octaves up-down',
                              '2 octaves assign',
                              '2 octaves random',
                              '3 octaves up',
                              '3 octaves down',
                              '3 octaves up-down',
                              '3 octaves assign',
                              '3 octaves random']);
tetraControl.pushItMode
    = partial(tetraSelector, ['normal',
                              'toggle']);

$(document).ready(function () {
    // The ready function is called multiple times because of the embedded svg documents (?)
    if (document.initialized) {
        return;
    }
    document.initialized = true;

    tetraControl.note
        .call(this,
              "osc-1-freq",
              "freq", 0, 0, false, false, false, false, false);
    tetraControl.fineTune
        .call(this,
              "osc-1-freq-fine",
              "fine", 1, 1, false, false, false, false, false);
    tetraControl.oscillatorShape
        .call(this,
              "osc-1-shape",
              "shape", 2, 2, false, false, false, false, false);
    tetraSpinnerWithRange(0, 127)
        .call(this,
              "glide-1",
              "glide", 3, 3, false, false, false, false, false);
    tetraControl.onOff
        .call(this,
              "osc-1-key",
              "key", 4, 4, false, false, false, false, false);
    tetraControl.note
        .call(this,
              "osc-2-freq",
              "freq", 6, 5, false, false, false, false, false);
    tetraControl.fineTune
        .call(this,
              "osc-2-freq-fine",
              "fine", 7, 6, false, false, false, false, false);
    tetraControl.oscillatorShape
        .call(this,
              "osc-2-shape",
              "shape", 8, 7, false, false, false, false, false);
    tetraSpinnerWithRange(0, 127)
        .call(this,
              "glide-2",
              "glide", 9, 8, false, false, false, false, false);
    tetraControl.onOff
        .call(this,
              "osc-2-key",
              "key", 10, 9, false, false, false, false, false);
    tetraControl.onOff
        .call(this,
              "sync",
              "sync", 12, 10, false, false, false, false, false);
    tetraControl.glideMode
        .call(this,
              "glide-mode",
              "glide mode", 13, 11, false, false, false, false, false);
    tetraSpinnerWithRange(0, 5)
        .call(this,
              "osc-slop",
              "slop", 14, 12, false, false, false, false, false);
    tetraSpinnerWithRange(0, 127)
        .call(this,
              "osc-mix",
              "mix", 16, 13, false, false, false, false, false);
    tetraSpinnerWithRange(0, 127)
        .call(this,
              "noise-level",
              "noise", 17, 14, false, false, false, false, false);
    tetraControl.note
        .call(this,
              "filter-freq",
              "freq", 20, 15, false, false, false, false, false);
    tetraSpinnerWithRange(0, 127)
        .call(this,
              "resonance",
              "reso", 21, 16, false, false, false, false, false);
    tetraSpinnerWithRange(0, 127)
        .call(this,
              "fil-key-amt",
              "key amt", 22, 17, false, false, false, false, false);
    tetraSpinnerWithRange(0, 127)
        .call(this,
              "fil-audio-mod",
              "audio mod", 23, 18, false, false, false, false, false);
    tetraControl.filterPoles
        .call(this,
              "filter-poles",
              "poles", 24, 19, false, false, false, false, false);
    tetraControl.posNegAmount
        .call(this,
              "filter-env-amt",
              "env amt", 25, 20, false, false, false, false, false);
    tetraSpinnerWithRange(0, 127)
        .call(this,
              "fil-env-vel-amt",
              "env vel amt", 26, 21, false, false, false, false, false);
    tetraSpinnerWithRange(0, 127)
        .call(this,
              "fil-del",
              "delay", 27, 22, false, false, false, false, false);
    tetraSpinnerWithRange(0, 127)
        .call(this,
              "fil-att",
              "attack", 28, 23, false, false, false, false, false);
    tetraSpinnerWithRange(0, 127)
        .call(this,
              "fil-dec",
              "decay", 29, 24, false, false, false, false, false);
    tetraSpinnerWithRange(0, 127)
        .call(this,
              "fil-sus",
              "sustain", 30, 25, false, false, false, false, false);
    tetraSpinnerWithRange(0, 127)
        .call(this,
              "fil-rel",
              "release", 31, 26, false, false, false, false, false);
    tetraSpinnerWithRange(0, 127)
        .call(this,
              "vca-level",
              "level", 32, 27, false, false, false, false, false);
    tetraSpinnerWithRange(0, 127)
        .call(this,
              "output-pan",
              "spread", 40, 28, false, false, false, false, false);
    tetraSpinnerWithRange(0, 127)
        .call(this,
              "preset-volume",
              "volume", 41, 29, false, false, true, false, true);
    tetraSpinnerWithRange(0, 127)
        .call(this,
              "vca-env-amount",
              "env amt", 33, 30, false, false, false, false, false);
    tetraSpinnerWithRange(0, 127)
        .call(this,
              "vca-env-vel-amt",
              "env vel amt", 34, 31, false, false, false, false, false);
    tetraSpinnerWithRange(0, 127)
        .call(this,
              "vca-del",
              "delay", 35, 32, false, false, false, false, false);
    tetraSpinnerWithRange(0, 127)
        .call(this,
              "vca-att",
              "attack", 36, 33, false, false, false, false, false);
    tetraSpinnerWithRange(0, 127)
        .call(this,
              "vca-dec",
              "decay", 37, 34, false, false, false, false, false);
    tetraSpinnerWithRange(0, 127)
        .call(this,
              "vca-sus",
              "sustain", 38, 35, false, false, false, false, false);
    tetraSpinnerWithRange(0, 127)
        .call(this,
              "vca-rel",
              "release", 39, 36, false, false, false, false, false);
    tetraControl.lfoFrequency
        .call(this,
              "lfo-1-freq",
              "freq", 42, 37, false, false, false, false, false);
    tetraControl.lfoShape
        .call(this,
              "lfo-1-shape",
              "shape", 43, 38, false, false, false, false, false);
    tetraSpinnerWithRange(0, 127)
        .call(this,
              "lfo-1-amt",
              "amount", 44, 39, false, false, false, false, false);
    tetraControl.modulationDestination
        .call(this,
              "lfo-1-dest",
              "dest", 45, 40, false, false, false, false, false);
    tetraControl.onOff
        .call(this,
              "lfo-1-sync",
              "sync", 46, 41, false, false, false, false, false);
    tetraControl.lfoFrequency
        .call(this,
              "lfo-2-freq",
              "freq", 47, 42, false, false, false, false, false);
    tetraControl.lfoShape
        .call(this,
              "lfo-2-shape",
              "shape", 48, 43, false, false, false, false, false);
    tetraSpinnerWithRange(0, 127)
        .call(this,
              "lfo-2-amt",
              "amount", 49, 44, false, false, false, false, false);
    tetraControl.modulationDestination
        .call(this,
              "lfo-2-dest",
              "dest", 50, 45, false, false, false, false, false);
    tetraControl.onOff
        .call(this,
              "lfo-2-sync",
              "sync", 51, 46, false, false, false, false, false);
    tetraControl.lfoFrequency
        .call(this,
              "lfo-3-freq",
              "freq", 52, 47, false, false, false, false, false);
    tetraControl.lfoShape
        .call(this,
              "lfo-3-shape",
              "shape", 53, 48, false, false, false, false, false);
    tetraSpinnerWithRange(0, 127)
        .call(this,
              "lfo-3-amt",
              "amount", 54, 49, false, false, false, false, false);
    tetraControl.modulationDestination
        .call(this,
              "lfo-3-dest",
              "dest", 55, 50, false, false, false, false, false);
    tetraControl.onOff
        .call(this,
              "lfo-3-sync",
              "sync", 56, 51, false, false, false, false, false);
    tetraControl.lfoFrequency
        .call(this,
              "lfo-4-freq",
              "freq", 57, 52, false, false, false, false, false);
    tetraControl.lfoShape
        .call(this,
              "lfo-4-shape",
              "shape", 58, 53, false, false, false, false, false);
    tetraSpinnerWithRange(0, 127)
        .call(this,
              "lfo-4-amt",
              "amount", 59, 54, false, false, false, false, false);
    tetraControl.modulationDestination
        .call(this,
              "lfo-4-dest",
              "dest", 60, 55, false, false, false, false, false);
    tetraControl.onOff
        .call(this,
              "lfo-4-sync",
              "sync", 61, 56, false, false, false, false, false);
    tetraControl.modulationDestination
        .call(this,
              "env3-dest",
              "dest", 62, 57, false, false, false, false, false);
    tetraControl.posNegAmount
        .call(this,
              "env3-amt",
              "amount", 63, 58, false, false, false, false, false);
    tetraSpinnerWithRange(0, 127)
        .call(this,
              "env3-vel-amt",
              "vel amt", 64, 59, false, false, false, false, false);
    tetraSpinnerWithRange(0, 127)
        .call(this,
              "env3-del",
              "delay", 65, 60, false, false, false, false, false);
    tetraSpinnerWithRange(0, 127)
        .call(this,
              "env3-att",
              "attack", 66, 61, false, false, false, false, false);
    tetraSpinnerWithRange(0, 127)
        .call(this,
              "env3-dec",
              "decay", 67, 62, false, false, false, false, false);
    tetraSpinnerWithRange(0, 127)
        .call(this,
              "env3-sus",
              "sustain", 68, 63, false, false, false, false, false);
    tetraSpinnerWithRange(0, 127)
        .call(this,
              "env3-rel",
              "release", 69, 64, false, false, false, false, false);
    tetraControl.modulationSource
        .call(this,
              "mod-source-1",
              "source", 71, 65, false, false, false, false, false);
    tetraControl.posNegAmount
        .call(this,
              "mod-amt-1",
              "amount", 72, 66, false, false, false, false, false);
    tetraControl.modulationDestination
        .call(this,
              "mod-dest-1",
              "dest", 73, 67, false, false, false, false, false);
    tetraControl.modulationSource
        .call(this,
              "mod-source-2",
              "source", 74, 68, false, false, false, false, false);
    tetraControl.posNegAmount
        .call(this,
              "mod-amt-2",
              "amount", 75, 69, false, false, false, false, false);
    tetraControl.modulationDestination
        .call(this,
              "mod-dest-2",
              "dest", 76, 70, false, false, false, false, false);
    tetraControl.modulationSource
        .call(this,
              "mod-source-3",
              "source", 77, 71, false, false, false, false, false);
    tetraControl.posNegAmount
        .call(this,
              "mod-amt-3",
              "amount", 78, 72, false, false, false, false, false);
    tetraControl.modulationDestination
        .call(this,
              "mod-dest-3",
              "dest", 79, 73, false, false, false, false, false);
    tetraControl.modulationSource
        .call(this,
              "mod-source-4",
              "source", 80, 74, false, false, false, false, false);
    tetraControl.posNegAmount
        .call(this,
              "mod-amt-4",
              "amount", 81, 75, false, false, false, false, false);
    tetraControl.modulationDestination
        .call(this,
              "mod-dest-4",
              "dest", 82, 76, false, false, false, false, false);
    tetraControl.modulationDestination
        .call(this,
              "seq-1-dest",
              "dest", 107, 77, false, false, false, false, false);
    tetraControl.modulationDestination
        .call(this,
              "seq-2-dest",
              "dest", 108, 78, false, false, false, false, false);
    tetraControl.modulationDestination
        .call(this,
              "seq-3-dest",
              "dest", 109, 79, false, false, false, false, false);
    tetraControl.modulationDestination
        .call(this,
              "seq-4-dest",
              "dest", 110, 80, false, false, false, false, false);
    tetraControl.posNegAmount
        .call(this,
              "mod-wheel-amt",
              "amount", 83, 81, false, false, false, false, false);
    tetraControl.modulationDestination
        .call(this,
              "mod-wheel-dest",
              "dest", 84, 82, false, false, false, false, false);
    tetraControl.posNegAmount
        .call(this,
              "pressure-amt",
              "amount", 85, 83, false, false, false, false, false);
    tetraControl.modulationDestination
        .call(this,
              "pressure-dest",
              "dest", 86, 84, false, false, false, false, false);
    tetraControl.posNegAmount
        .call(this,
              "breath-amt",
              "amount", 87, 85, false, false, false, false, false);
    tetraControl.modulationDestination
        .call(this,
              "breath-dest",
              "dest", 88, 86, false, false, false, false, false);
    tetraControl.posNegAmount
        .call(this,
              "velocity-amt",
              "amount", 89, 87, false, false, false, false, false);
    tetraControl.modulationDestination
        .call(this,
              "velocity-dest",
              "dest", 90, 88, false, false, false, false, false);
    tetraControl.posNegAmount
        .call(this,
              "foot-amt",
              "amount", 91, 89, false, false, false, false, false);
    tetraControl.modulationDestination
        .call(this,
              "foot-dest",
              "dest", 92, 90, false, false, false, false, false);
    tetraSpinnerWithRange(30, 250)
        .call(this,
              "tempo",
              "tempo", 101, 91, false, false, false, false, false);
    tetraControl.timeSignature
        .call(this,
              "time-sig",
              "sig", 102, 92, false, false, false, false, false);
    tetraSpinnerWithRange(0, 12)
        .call(this,
              "pbend-range",
              "pb range", 15, 93, false, false, false, false, false);
    tetraControl.sequencerTrigger
        .call(this,
              "seq-trigger",
              "SEQ TRIGGER", 105, 94, false, false, false, false, false);
    tetraControl.keyMode
        .call(this,
              "unison-assign",
              "UNISON ASSIGN", 94, 95, false, false, false, false, false);
    tetraControl.unisonMode
        .call(this,
              "unison-mode",
              "UNISON MODE", 93, 96, false, false, false, false, false);
    tetraControl.arpeggiatorMode
        .call(this,
              "arp-mode",
              "ARP MODE", 103, 97, false, false, false, false, false);
    tetraControl.onOff
        .call(this,
              "env3-repeat",
              "repeat", 70, 98, false, false, false, false, false);
    tetraControl.onOff
        .call(this,
              "unison-on-off",
              "unison", 95, 99, false, false, true, true, true);
    tetraControl.onOff
        .call(this,
              "arp-on-off",
              "arp", 104, 100, false, false, true, true, true);
    tetraControl.onOff
        .call(this,
              "seq-on-off",
              "seq", 106, 101, false, false, false, false, false);
    tetraSpinnerWithRange(0, 183)
        .call(this,
              "param-enc-sel-v1",
              "PARAM ENC SEL V1", 111, 105, true, false, true, true, true);
    tetraSpinnerWithRange(0, 183)
        .call(this,
              "param-enc-sel-v2",
              "PARAM ENC SEL V2", 112, 106, true, false, true, true, true);
    tetraSpinnerWithRange(0, 183)
        .call(this,
              "param-enc-sel-v3",
              "PARAM ENC SEL V3", 113, 107, true, false, true, true, true);
    tetraSpinnerWithRange(0, 183)
        .call(this,
              "param-enc-sel-v4",
              "PARAM ENC SEL V4", 114, 108, true, false, true, true, true);
    tetraSpinnerWithRange(0, 127)
        .call(this,
              "feedback-gain",
              "FEEDBACK GAIN", 19, 110, false, false, false, false, false);
    tetraControl.note
        .call(this,
              "push-it-note",
              "PUSH IT NOTE", 96, 111, false, false, false, false, false);
    tetraSpinnerWithRange(0, 127)
        .call(this,
              "push-it-vel",
              "PUSH IT VEL", 97, 112, false, false, false, false, false);
    tetraControl.pushItMode
        .call(this,
              "push-it-mode",
              "PUSH IT MODE", 98, 113, false, false, false, false, false);
    tetraSpinnerWithRange(0, 127)
        .call(this,
              "sub-osc-1-lvl",
              "SUB OSC 1 LVL", 5, 114, false, false, false, false, false);
    tetraSpinnerWithRange(0, 127)
        .call(this,
              "sub-osc-2-lvl",
              "SUB OSC 2 LVL", 11, 115, false, false, false, false, false);
    tetraSpinnerWithRange(0, 127)
        .call(this,
              "feedback-vol",
              "FEEDBACK VOL", 18, 116, false, false, false, false, false);
    tetraSpinnerWithRange(0, 2)
        .call(this,
              "editor-byte",
              "EDITOR BYTE", 117, 117, true, false, false, false, false);
    tetraControl.note
        .call(this,
              "kbd-split-point",
              "KBD SPLIT POINT", 99, 118, true, false, false, false, false);
    tetraSpinnerWithRange(0, 2)
        .call(this,
              "kbd-mode",
              "KBD MODE", 100, 119, true, false, false, false, false);

    var host = document.location.host.replace(/:.*/, "");
    socket = new io.Socket(host); 
    function reconnect() {
        console.log('connecting to', host);
        socket.connect();
    }
    socket.on('connect_failed', function(e) {
        console.log('connection failed, reconnecting');
        setTimeout(reconnect, 500);
    });
    socket.on('connect', function() {
        console.log('socket connected');
    });
    socket.on('message', processSocketMessage);
    socket.on('disconnect', function() {
        console.log('socket disconnect, reconnecting');
        setTimeout(reconnect, 500);
    });
    $('button').bind('change', function () {
        sendParameterChange(this.id, this.control.getInternalValue());
    });

    reconnect();

    _.each(_.range(4), function (seq) {
        _.each(_.range(16), function (step) {
            var id = "seq-track-" + (seq + 1) + "-step-" + (step + 1);
            var spinner = tetraSpinnerWithRange(0, 125)
                .call(this,
                      id,
                      undefined, 120 + seq * 16 + step, 120 + seq * 16 + step,
                      false, false, false, false, false);
            spinner.resetToggler = tetraToggler(undefined,
                                                id + "-reset",
                                                "reset", 120 + seq * 16 + step, 120 + seq * 16 + step,
                                                false, false, false, false, false);
            spinner.resetToggler.spinner = spinner;
            $(spinner.resetToggler.getButtonElement()).bind('click', function () {
                var spinner = this.control.spinner;
                var value = this.control.getInternalValue() ? 126 : spinner.getInternalValue();
                if (value > 125) {
                    spinner.setDisabled(true);
                } else {
                    spinner.setDisabled(false);
                }
                if (spinner.restToggler) {
                    spinner.restToggler.setInternalValue(0);
                }
                sendParameterChange(spinner.getButtonElement().id, value);
            });
            if (seq == 0) {
                spinner.restToggler = tetraToggler(undefined,
                                                   id + "-rest",
                                                   "rest", 120 + seq * 16 + step, 120 + seq * 16 + step,
                                                   false, false, false, false, false);
                spinner.restToggler.spinner = spinner;
                $(spinner.restToggler.getButtonElement()).bind('click', function () {
                    var spinner = this.control.spinner;
                    var value = this.control.getInternalValue() ? 127 : spinner.getInternalValue();
                    if (value > 125) {
                        spinner.setDisabled(true);
                    } else {
                        spinner.setDisabled(false);
                    }
                    spinner.resetToggler.setInternalValue(0);
                    sendParameterChange(spinner.getButtonElement().id, value);
                });
            }
            spinner.realSetInternalValue = spinner.setInternalValue;
            spinner.setInternalValue = function (value) {
                console.log('spinner value', value);
                if (value > 125) {
                    this.setDisabled(true);
                } else {
                    this.setDisabled(false);
                }
                this.resetToggler.setInternalValue(value == 126);
                if (this.restToggler) {
                    this.restToggler.setInternalValue(value == 127);
                }
                this.realSetInternalValue(value);
            }
        });
    });

    $('#preset-selector').bind('click', function () {
        var presetName = $(this).val();
        console.log('sending preset', allPresets[presetName].parameters);
        socket.send(serializeJSON(['preset', allPresets[presetName]]));
    });

    function showPresetsPage()
    {
        console.log('show presets page');
        $('#preset-selector').empty();
        $.getJSON("tetra-presets.json", function (presets, status) {
            if (status != 'success') {
                console.log('error loading presets:', status);
            } else {
                allPresets = {};
                allLabels = {};
                console.log('got', presets.length, 'presets');
                $('#preset-selector')
                    .append(_.map(presets,
                                  function (preset) {
                                      _.each(preset.labels, function (label) {
                                          allLabels[label] = allLabels[label] || { count: 0 };
                                          allLabels[label].count++;
                                      });
                                      allPresets[preset.name] = preset;
                                      return DOM.OPTION(null, preset.name)
                                  }));
            }
            $('#presets #filter-labels')
                .empty()
                .append(_.map(allLabels, function (value, key) {
                    var button = DOM.BUTTON({ label: key }, key);
                    $(button).bind('click', function () {
                        console.log('button', $(this).attr('label'), 'clicked');
                        if ($(this).attr('active')) {
                            $(this)
                                .removeAttr('active')
                                .removeClass('active');
                        } else {
                            $(this)
                                .attr('active', true)
                                .addClass('active');
                        }
                    });
                    return button;
                }));
        });
    }

    $('#presets').bind('show', showPresetsPage);

    $('#osc-env').bind('show', function () {
        // Apparently, initializing SVG documents takes a moment, so defer
        // trying to bind handlers until that has happened.
        setTimeout(initAdsr, 500);
    });

    $('#seq button.seq-spinner')
        .each(function () {
            $('#' + this.id + '-rest')
                .bind('click', function () {
                    console.log(this.id, 'clicked, value', this.control.getInternalValue());
                });
            $('#' + this.id + '-reset')
                .bind('click', function () {
                    console.log(this.id, 'clicked, value', this.control.getInternalValue());
                });
        });

    // Oh wow, I so love writing multi-page frameworks - Here is another one! :)

    $('div.page div.title').each(function () {
        var button = DOM.BUTTON(null, $(this).html());
        button.page = this.parentNode;
        $(button).bind('click', function () {
            $('div.page').css('display', 'none');
            $('#menu button').removeClass('active');
            $(this).addClass('active');
            $(this.page)
                .css('display', 'block')
                .trigger('show');
            currentPath = document.location.hash = '#' + this.page.id;
        });
        $('#menu').append(button);
    });

    $('#menu').bind('goto', function (event, id) {
        $(this).find('button').each(function () {
            if (this.page.id == id) {
                $(this).trigger('click');
            }
        });
    });

    if (document.location.hash == "") {
        document.location.hash = "#osc-env";
    }
    console.log('starting at', document.location.hash);

    function pollForChanges()
    {
        // check for "back" button usage
        if (document.location.hash != currentPath) {
            var path = document.location.hash.substr(1).split('/');
            var id = path.shift();
            $('#menu').trigger('goto', id);
        }
    }

    setInterval(pollForChanges, 250);
});

function initAdsr() {
    function setupAdsr() {
        var that = this.getSVGDocument();
        that.paramElements = this.parentNode.getAttribute('adsr-params').split(',');
        that.paramToKey = {};
        var keys = ['delay', 'attack', 'decay', 'sustain', 'release'];
        that.set = function (key, parameter, value) {
            this.params[key] = value;
            try {
                this.drawEnvelope();
            }
            catch (e) {
                console.log('error updating adsr graph:', e);
            }
        }
        map(function (id, key) {
            that.params[key] = $('#' + id)[0].control.getInternalValue();
            adsrParam[id] = bind(that.set, that, key);
            $('#' + id).bind('change', function () {
                that.params[key] = this.control.getInternalValue();
                try {
                    this.drawEnvelope();
                }
                catch (e) {
                    console.log('error updating adsr graph:', e);
                }
            });
        }, that.paramElements, keys);
        $(that).bind('change', function () {
            var that = this;
            map(function (key, id) {
                var control = $('#' + id)[0].control;
                if (control.getInternalValue() != that.params[key]) {
                    control.setInternalValue(that.params[key]);
                    socket.send(serializeJSON(['set', id, that.params[key]]));
                }
            },
                keys, this.paramElements);
        });
        try {
            this.drawEnvelope();
        }
        catch (e) {
            console.log('error updating adsr graph:', e);
        }
    }
    $('.adsr-graph embed').map(setupAdsr);
}

