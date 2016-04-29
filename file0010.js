var texts = new Array();
var states = new Array();

texts['fold000001'] = '<a href="javascript:fold(\'fold000001\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 1 to line 24</i>';
states['fold000001'] = false;
texts['fold000029'] = '<a href="javascript:fold(\'fold000029\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 29 to line 43</i>';
states['fold000029'] = false;
texts['fold000048'] = '<a href="javascript:fold(\'fold000048\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 48 to line 64</i>';
states['fold000048'] = false;
texts['fold000067'] = '<a href="javascript:fold(\'fold000067\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 67 to line 71</i>';
states['fold000067'] = false;
texts['fold000074'] = '<a href="javascript:fold(\'fold000074\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 74 to line 82</i>';
states['fold000074'] = false;
texts['fold000084'] = '<a href="javascript:fold(\'fold000084\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 84 to line 85</i>';
states['fold000084'] = false;
texts['fold000087'] = '<a href="javascript:fold(\'fold000087\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 87 to line 88</i>';
states['fold000087'] = false;
texts['fold000090'] = '<a href="javascript:fold(\'fold000090\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 90 to line 91</i>';
states['fold000090'] = false;
texts['fold000093'] = '<a href="javascript:fold(\'fold000093\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 93 to line 98</i>';
states['fold000093'] = false;
texts['fold000103'] = '<a href="javascript:fold(\'fold000103\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 103 to line 104</i>';
states['fold000103'] = false;
texts['fold000110'] = '<a href="javascript:fold(\'fold000110\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 110 to line 110</i>';
states['fold000110'] = false;
texts['fold000112'] = '<a href="javascript:fold(\'fold000112\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 112 to line 114</i>';
states['fold000112'] = false;
texts['fold000119'] = '<a href="javascript:fold(\'fold000119\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 119 to line 141</i>';
states['fold000119'] = false;
texts['fold000145'] = '<a href="javascript:fold(\'fold000145\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 145 to line 185</i>';
states['fold000145'] = false;
texts['fold000187'] = '<a href="javascript:fold(\'fold000187\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 187 to line 196</i>';
states['fold000187'] = false;
texts['fold000198'] = '<a href="javascript:fold(\'fold000198\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 198 to line 200</i>';
states['fold000198'] = false;
texts['fold000204'] = '<a href="javascript:fold(\'fold000204\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 204 to line 239</i>';
states['fold000204'] = false;
texts['fold000241'] = '<a href="javascript:fold(\'fold000241\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 241 to line 241</i>';
states['fold000241'] = false;
texts['fold000243'] = '<a href="javascript:fold(\'fold000243\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 243 to line 244</i>';
states['fold000243'] = false;
texts['fold000246'] = '<a href="javascript:fold(\'fold000246\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 246 to line 249</i>';
states['fold000246'] = false;
texts['fold000253'] = '<a href="javascript:fold(\'fold000253\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 253 to line 329</i>';
states['fold000253'] = false;
texts['fold000331'] = '<a href="javascript:fold(\'fold000331\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 331 to line 368</i>';
states['fold000331'] = false;

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
