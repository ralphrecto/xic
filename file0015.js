var texts = new Array();
var states = new Array();

texts['fold000001'] = '<a href="javascript:fold(\'fold000001\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 1 to line 4</i>';
states['fold000001'] = false;
texts['fold000006'] = '<a href="javascript:fold(\'fold000006\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 6 to line 6</i>';
states['fold000006'] = false;
texts['fold000008'] = '<a href="javascript:fold(\'fold000008\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 8 to line 8</i>';
states['fold000008'] = false;
texts['fold000010'] = '<a href="javascript:fold(\'fold000010\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 10 to line 10</i>';
states['fold000010'] = false;
texts['fold000012'] = '<a href="javascript:fold(\'fold000012\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 12 to line 13</i>';
states['fold000012'] = false;
texts['fold000015'] = '<a href="javascript:fold(\'fold000015\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 15 to line 15</i>';
states['fold000015'] = false;
texts['fold000017'] = '<a href="javascript:fold(\'fold000017\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 17 to line 17</i>';
states['fold000017'] = false;
texts['fold000029'] = '<a href="javascript:fold(\'fold000029\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 29 to line 29</i>';
states['fold000029'] = false;
texts['fold000033'] = '<a href="javascript:fold(\'fold000033\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 33 to line 33</i>';
states['fold000033'] = false;
texts['fold000037'] = '<a href="javascript:fold(\'fold000037\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 37 to line 37</i>';
states['fold000037'] = false;
texts['fold000041'] = '<a href="javascript:fold(\'fold000041\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 41 to line 41</i>';
states['fold000041'] = false;
texts['fold000043'] = '<a href="javascript:fold(\'fold000043\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 43 to line 43</i>';
states['fold000043'] = false;
texts['fold000047'] = '<a href="javascript:fold(\'fold000047\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 47 to line 47</i>';
states['fold000047'] = false;
texts['fold000049'] = '<a href="javascript:fold(\'fold000049\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 49 to line 49</i>';
states['fold000049'] = false;
texts['fold000051'] = '<a href="javascript:fold(\'fold000051\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 51 to line 54</i>';
states['fold000051'] = false;
texts['fold000056'] = '<a href="javascript:fold(\'fold000056\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 56 to line 56</i>';
states['fold000056'] = false;
texts['fold000061'] = '<a href="javascript:fold(\'fold000061\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 61 to line 61</i>';
states['fold000061'] = false;
texts['fold000069'] = '<a href="javascript:fold(\'fold000069\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 69 to line 70</i>';
states['fold000069'] = false;
texts['fold000089'] = '<a href="javascript:fold(\'fold000089\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 89 to line 125</i>';
states['fold000089'] = false;
texts['fold000144'] = '<a href="javascript:fold(\'fold000144\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 144 to line 145</i>';
states['fold000144'] = false;
texts['fold000157'] = '<a href="javascript:fold(\'fold000157\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 157 to line 158</i>';
states['fold000157'] = false;
texts['fold000170'] = '<a href="javascript:fold(\'fold000170\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 170 to line 171</i>';
states['fold000170'] = false;

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
