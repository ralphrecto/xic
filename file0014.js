var texts = new Array();
var states = new Array();

texts['fold000001'] = '<a href="javascript:fold(\'fold000001\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 1 to line 40</i>';
states['fold000001'] = false;
texts['fold000042'] = '<a href="javascript:fold(\'fold000042\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 42 to line 42</i>';
states['fold000042'] = false;
texts['fold000044'] = '<a href="javascript:fold(\'fold000044\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 44 to line 53</i>';
states['fold000044'] = false;
texts['fold000055'] = '<a href="javascript:fold(\'fold000055\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 55 to line 94</i>';
states['fold000055'] = false;
texts['fold000098'] = '<a href="javascript:fold(\'fold000098\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 98 to line 102</i>';
states['fold000098'] = false;
texts['fold000112'] = '<a href="javascript:fold(\'fold000112\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 112 to line 256</i>';
states['fold000112'] = false;
texts['fold000259'] = '<a href="javascript:fold(\'fold000259\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 259 to line 273</i>';
states['fold000259'] = false;
texts['fold000276'] = '<a href="javascript:fold(\'fold000276\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 276 to line 281</i>';
states['fold000276'] = false;
texts['fold000283'] = '<a href="javascript:fold(\'fold000283\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 283 to line 345</i>';
states['fold000283'] = false;
texts['fold000347'] = '<a href="javascript:fold(\'fold000347\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 347 to line 382</i>';
states['fold000347'] = false;
texts['fold000384'] = '<a href="javascript:fold(\'fold000384\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 384 to line 442</i>';
states['fold000384'] = false;
texts['fold000444'] = '<a href="javascript:fold(\'fold000444\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 444 to line 519</i>';
states['fold000444'] = false;
texts['fold000521'] = '<a href="javascript:fold(\'fold000521\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 521 to line 524</i>';
states['fold000521'] = false;
texts['fold000526'] = '<a href="javascript:fold(\'fold000526\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 526 to line 601</i>';
states['fold000526'] = false;
texts['fold000603'] = '<a href="javascript:fold(\'fold000603\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 603 to line 614</i>';
states['fold000603'] = false;
texts['fold000616'] = '<a href="javascript:fold(\'fold000616\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 616 to line 624</i>';
states['fold000616'] = false;
texts['fold000626'] = '<a href="javascript:fold(\'fold000626\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 626 to line 637</i>';
states['fold000626'] = false;
texts['fold000639'] = '<a href="javascript:fold(\'fold000639\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 639 to line 650</i>';
states['fold000639'] = false;
texts['fold000652'] = '<a href="javascript:fold(\'fold000652\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 652 to line 652</i>';
states['fold000652'] = false;
texts['fold000654'] = '<a href="javascript:fold(\'fold000654\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 654 to line 714</i>';
states['fold000654'] = false;

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
