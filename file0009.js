var texts = new Array();
var states = new Array();

texts['fold000001'] = '<a href="javascript:fold(\'fold000001\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 1 to line 230</i>';
states['fold000001'] = false;
texts['fold000232'] = '<a href="javascript:fold(\'fold000232\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 232 to line 232</i>';
states['fold000232'] = false;
texts['fold000234'] = '<a href="javascript:fold(\'fold000234\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 234 to line 235</i>';
states['fold000234'] = false;
texts['fold000237'] = '<a href="javascript:fold(\'fold000237\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 237 to line 237</i>';
states['fold000237'] = false;
texts['fold000239'] = '<a href="javascript:fold(\'fold000239\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 239 to line 242</i>';
states['fold000239'] = false;
texts['fold000244'] = '<a href="javascript:fold(\'fold000244\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 244 to line 244</i>';
states['fold000244'] = false;
texts['fold000246'] = '<a href="javascript:fold(\'fold000246\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 246 to line 254</i>';
states['fold000246'] = false;
texts['fold000256'] = '<a href="javascript:fold(\'fold000256\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 256 to line 270</i>';
states['fold000256'] = false;
texts['fold000272'] = '<a href="javascript:fold(\'fold000272\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 272 to line 287</i>';
states['fold000272'] = false;
texts['fold000289'] = '<a href="javascript:fold(\'fold000289\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 289 to line 292</i>';
states['fold000289'] = false;
texts['fold000294'] = '<a href="javascript:fold(\'fold000294\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 294 to line 295</i>';
states['fold000294'] = false;
texts['fold000297'] = '<a href="javascript:fold(\'fold000297\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 297 to line 300</i>';
states['fold000297'] = false;
texts['fold000302'] = '<a href="javascript:fold(\'fold000302\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 302 to line 305</i>';
states['fold000302'] = false;
texts['fold000307'] = '<a href="javascript:fold(\'fold000307\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 307 to line 309</i>';
states['fold000307'] = false;
texts['fold000311'] = '<a href="javascript:fold(\'fold000311\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 311 to line 1784</i>';
states['fold000311'] = false;

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
