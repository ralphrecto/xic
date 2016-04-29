var texts = new Array();
var states = new Array();

texts['fold000001'] = '<a href="javascript:fold(\'fold000001\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 1 to line 69</i>';
states['fold000001'] = false;
texts['fold000074'] = '<a href="javascript:fold(\'fold000074\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 74 to line 78</i>';
states['fold000074'] = false;
texts['fold000081'] = '<a href="javascript:fold(\'fold000081\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 81 to line 82</i>';
states['fold000081'] = false;
texts['fold000084'] = '<a href="javascript:fold(\'fold000084\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 84 to line 84</i>';
states['fold000084'] = false;
texts['fold000086'] = '<a href="javascript:fold(\'fold000086\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 86 to line 89</i>';
states['fold000086'] = false;
texts['fold000093'] = '<a href="javascript:fold(\'fold000093\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 93 to line 93</i>';
states['fold000093'] = false;
texts['fold000098'] = '<a href="javascript:fold(\'fold000098\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 98 to line 98</i>';
states['fold000098'] = false;
texts['fold000101'] = '<a href="javascript:fold(\'fold000101\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 101 to line 101</i>';
states['fold000101'] = false;
texts['fold000103'] = '<a href="javascript:fold(\'fold000103\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 103 to line 106</i>';
states['fold000103'] = false;
texts['fold000108'] = '<a href="javascript:fold(\'fold000108\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 108 to line 109</i>';
states['fold000108'] = false;
texts['fold000111'] = '<a href="javascript:fold(\'fold000111\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 111 to line 111</i>';
states['fold000111'] = false;
texts['fold000113'] = '<a href="javascript:fold(\'fold000113\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 113 to line 114</i>';
states['fold000113'] = false;
texts['fold000116'] = '<a href="javascript:fold(\'fold000116\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 116 to line 121</i>';
states['fold000116'] = false;
texts['fold000125'] = '<a href="javascript:fold(\'fold000125\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 125 to line 125</i>';
states['fold000125'] = false;
texts['fold000128'] = '<a href="javascript:fold(\'fold000128\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 128 to line 130</i>';
states['fold000128'] = false;
texts['fold000134'] = '<a href="javascript:fold(\'fold000134\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 134 to line 135</i>';
states['fold000134'] = false;
texts['fold000137'] = '<a href="javascript:fold(\'fold000137\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 137 to line 140</i>';
states['fold000137'] = false;
texts['fold000144'] = '<a href="javascript:fold(\'fold000144\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 144 to line 144</i>';
states['fold000144'] = false;
texts['fold000149'] = '<a href="javascript:fold(\'fold000149\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 149 to line 149</i>';
states['fold000149'] = false;
texts['fold000153'] = '<a href="javascript:fold(\'fold000153\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 153 to line 153</i>';
states['fold000153'] = false;
texts['fold000156'] = '<a href="javascript:fold(\'fold000156\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 156 to line 157</i>';
states['fold000156'] = false;
texts['fold000160'] = '<a href="javascript:fold(\'fold000160\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 160 to line 160</i>';
states['fold000160'] = false;
texts['fold000162'] = '<a href="javascript:fold(\'fold000162\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 162 to line 163</i>';
states['fold000162'] = false;

function fold(id) {
  tmp = document.getElementById(id).innerHTML;
  document.getElementById(id).innerHTML = texts[id];
  texts[id] = tmp;
  states[id] = !(states[id]);
}

function unfoldAll() {
  for (key in states) {
    if (states[key]) {
      fold(key);
    }
  }
}

function foldAll() {
  for (key in states) {
    if (!(states[key])) {
      fold(key);
    }
  }
}
