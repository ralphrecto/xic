var texts = new Array();
var states = new Array();

texts['fold000001'] = '<a href="javascript:fold(\'fold000001\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 1 to line 16</i>';
states['fold000001'] = false;
texts['fold000031'] = '<a href="javascript:fold(\'fold000031\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 31 to line 33</i>';
states['fold000031'] = false;
texts['fold000037'] = '<a href="javascript:fold(\'fold000037\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 37 to line 38</i>';
states['fold000037'] = false;
texts['fold000040'] = '<a href="javascript:fold(\'fold000040\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 40 to line 41</i>';
states['fold000040'] = false;
texts['fold000043'] = '<a href="javascript:fold(\'fold000043\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 43 to line 44</i>';
states['fold000043'] = false;
texts['fold000046'] = '<a href="javascript:fold(\'fold000046\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 46 to line 47</i>';
states['fold000046'] = false;
texts['fold000050'] = '<a href="javascript:fold(\'fold000050\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 50 to line 67</i>';
states['fold000050'] = false;
texts['fold000070'] = '<a href="javascript:fold(\'fold000070\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 70 to line 82</i>';
states['fold000070'] = false;
texts['fold000084'] = '<a href="javascript:fold(\'fold000084\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 84 to line 90</i>';
states['fold000084'] = false;
texts['fold000096'] = '<a href="javascript:fold(\'fold000096\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 96 to line 123</i>';
states['fold000096'] = false;
texts['fold000129'] = '<a href="javascript:fold(\'fold000129\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 129 to line 171</i>';
states['fold000129'] = false;
texts['fold000174'] = '<a href="javascript:fold(\'fold000174\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 174 to line 174</i>';
states['fold000174'] = false;
texts['fold000177'] = '<a href="javascript:fold(\'fold000177\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 177 to line 180</i>';
states['fold000177'] = false;
texts['fold000184'] = '<a href="javascript:fold(\'fold000184\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 184 to line 225</i>';
states['fold000184'] = false;

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
