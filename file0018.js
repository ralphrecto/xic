var texts = new Array();
var states = new Array();

texts['fold000001'] = '<a href="javascript:fold(\'fold000001\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 1 to line 5</i>';
states['fold000001'] = false;
texts['fold000007'] = '<a href="javascript:fold(\'fold000007\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 7 to line 15</i>';
states['fold000007'] = false;
texts['fold000019'] = '<a href="javascript:fold(\'fold000019\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 19 to line 20</i>';
states['fold000019'] = false;
texts['fold000023'] = '<a href="javascript:fold(\'fold000023\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 23 to line 23</i>';
states['fold000023'] = false;
texts['fold000025'] = '<a href="javascript:fold(\'fold000025\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 25 to line 31</i>';
states['fold000025'] = false;
texts['fold000033'] = '<a href="javascript:fold(\'fold000033\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 33 to line 42</i>';
states['fold000033'] = false;
texts['fold000047'] = '<a href="javascript:fold(\'fold000047\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 47 to line 48</i>';
states['fold000047'] = false;

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
