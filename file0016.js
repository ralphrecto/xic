var texts = new Array();
var states = new Array();

texts['fold000001'] = '<a href="javascript:fold(\'fold000001\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 1 to line 4</i>';
states['fold000001'] = false;
texts['fold000013'] = '<a href="javascript:fold(\'fold000013\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 13 to line 13</i>';
states['fold000013'] = false;
texts['fold000033'] = '<a href="javascript:fold(\'fold000033\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 33 to line 33</i>';
states['fold000033'] = false;
texts['fold000037'] = '<a href="javascript:fold(\'fold000037\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 37 to line 37</i>';
states['fold000037'] = false;
texts['fold000047'] = '<a href="javascript:fold(\'fold000047\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 47 to line 47</i>';
states['fold000047'] = false;
texts['fold000049'] = '<a href="javascript:fold(\'fold000049\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 49 to line 49</i>';
states['fold000049'] = false;
texts['fold000051'] = '<a href="javascript:fold(\'fold000051\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 51 to line 56</i>';
states['fold000051'] = false;
texts['fold000058'] = '<a href="javascript:fold(\'fold000058\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 58 to line 152</i>';
states['fold000058'] = false;
texts['fold000154'] = '<a href="javascript:fold(\'fold000154\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 154 to line 161</i>';
states['fold000154'] = false;
texts['fold000163'] = '<a href="javascript:fold(\'fold000163\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 163 to line 171</i>';
states['fold000163'] = false;

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
