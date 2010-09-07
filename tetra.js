var partial = MochiKit.Base.partial;
var map = MochiKit.Base.map;

var tetraControl = {};

function tetraToggler(id, title)
{
    if (document.getElementById(id)) {
        return new Toggler(id, { text: title, width: 60, height: 24, fontSize: 9 });
    }
}

function tetraSelector(values, id)
{
    if (document.getElementById(id)) {
        return new Selector(id,
                            { items: values, height: 24 });
    }
}

function tetraSpinner(externalMapping,
                      id, title)
{
    if (document.getElementById(id)) {
        return new Spinner(id,
                           { title: title,
                                   size: 40,
                                   height: 60,
                                   width: 80,
                                   externalMapping: externalMapping });
    }
}

function tetraSpinnerWithRange (min, max)
{
    return partial(tetraSpinner,
                   { fromFactor: function (factor) {
                           return min + Math.floor(factor * (max - min));
                       },
                           toFactor: function (value) {
                           return (value - min) / (max - min);
                       }
                   });
};

var notes = [ "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B" ];

tetraControl.note
    = partial(tetraSpinner,
              { fromFactor: function (factor) {
                      var index = Math.floor(factor * 120);
                      var note = index % 12;
                      var octave = Math.floor(index / 12);
                      return notes[note] + octave;
                  },
                toFactor: function (value) {
                      return 0;
                  }
              });
                                    
tetraControl.fineTune
    = partial(tetraSpinner,
              { fromFactor: function (factor) {
                      var index = Math.floor(factor * 100);
                      return -50 + index;
                  },
                toFactor: function (value) {
                      var index = 50 + value;
                      return Math.min(100, index) / 100;
                  }
              });
                                        
tetraControl.oscillatorShape
    = partial(tetraSpinner,
              { fromFactor: function (factor) {
                      var index = Math.floor(factor * 103);
                      switch (index) {
                      case 0: return "OFF";
                      case 1: return "SAW";
                      case 2: return "TRI";
                      case 3: return "TRI/SAW";
                      default: return "P" + (index - 4);
                      }
                  },
                  toFactor: function (value) {
                  }
              });
                                       
tetraControl.onOff = partial(tetraToggler);
tetraControl.glideMode = partial(tetraSelector, [ 'rate', 'rate auto', 'time', 'time auto' ]);
tetraControl.filterPoles = partial(tetraSelector, [ '2 pole', '4 pole' ]);
tetraControl.posNegAmount
    = partial(tetraSpinner,
              { fromFactor: function (factor) {
                      var index = Math.floor(factor * 254);
                      return -127 + index;
                  },
                toFactor: function (value) {
                  }
              });
tetraControl.lfoFrequency
    = partial(tetraSpinner,
              { fromFactor: function (factor) {
                      var index = Math.floor(factor * 166);
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
                  },
                toFactor: function (value) {
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
                              'assign']);
tetraControl.pushItMode
    = partial(tetraSelector, ['normal',
                              'toggle']);


$(document).ready(function () {
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
        "amt", 44, 39, false, false, false, false, false);
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
        "amt", 49, 44, false, false, false, false, false);
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
        "amt", 54, 49, false, false, false, false, false);
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
        "amt", 59, 54, false, false, false, false, false);
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
        "amt", 63, 58, false, false, false, false, false);
tetraSpinnerWithRange(0, 127)
  .call(this,
        "env3-vel-amt",
        "vel amt", 64, 59, false, false, false, false, false);
tetraSpinnerWithRange(0, 127)
  .call(this,
        "env3-delay",
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
        "amt", 72, 66, false, false, false, false, false);
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
        "amt", 75, 69, false, false, false, false, false);
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
        "amt", 78, 72, false, false, false, false, false);
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
        "amt", 81, 75, false, false, false, false, false);
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
        "amt", 83, 81, false, false, false, false, false);
tetraControl.modulationDestination
  .call(this,
        "mod-wheel-dest",
        "dest", 84, 82, false, false, false, false, false);
tetraControl.posNegAmount
  .call(this,
        "pressure-amt",
        "amt", 85, 83, false, false, false, false, false);
tetraControl.modulationDestination
  .call(this,
        "pressure-dest",
        "dest", 86, 84, false, false, false, false, false);
tetraControl.posNegAmount
  .call(this,
        "breath-amt",
        "amt", 87, 85, false, false, false, false, false);
tetraControl.modulationDestination
  .call(this,
        "breath-dest",
        "dest", 88, 86, false, false, false, false, false);
tetraControl.posNegAmount
  .call(this,
        "velocity-amt",
        "amt", 89, 87, false, false, false, false, false);
tetraControl.modulationDestination
  .call(this,
        "velocity-dest",
        "dest", 90, 88, false, false, false, false, false);
tetraControl.posNegAmount
  .call(this,
        "foot-amt",
        "amt", 91, 89, false, false, false, false, false);
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
    });
