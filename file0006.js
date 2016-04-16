var texts = new Array();
var states = new Array();

texts['fold000001'] = '<a href="javascript:fold(\'fold000001\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 1 to line 4</i>';
states['fold000001'] = false;
texts['fold000011'] = '<a href="javascript:fold(\'fold000011\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 11 to line 11</i>';
states['fold000011'] = false;
texts['fold000015'] = '<a href="javascript:fold(\'fold000015\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 15 to line 16</i>';
states['fold000015'] = false;
texts['fold000018'] = '<a href="javascript:fold(\'fold000018\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 18 to line 21</i>';
states['fold000018'] = false;
texts['fold000027'] = '<a href="javascript:fold(\'fold000027\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 27 to line 32</i>';
states['fold000027'] = false;
texts['fold000037'] = '<a href="javascript:fold(\'fold000037\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 37 to line 42</i>';
states['fold000037'] = false;
texts['fold000049'] = '<a href="javascript:fold(\'fold000049\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 49 to line 50</i>';
states['fold000049'] = false;

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
