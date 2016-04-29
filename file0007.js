var texts = new Array();
var states = new Array();

texts['fold000001'] = '<a href="javascript:fold(\'fold000001\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 1 to line 510</i>';
states['fold000001'] = false;
texts['fold000513'] = '<a href="javascript:fold(\'fold000513\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 513 to line 524</i>';
states['fold000513'] = false;
texts['fold000526'] = '<a href="javascript:fold(\'fold000526\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 526 to line 526</i>';
states['fold000526'] = false;
texts['fold000529'] = '<a href="javascript:fold(\'fold000529\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 529 to line 542</i>';
states['fold000529'] = false;
texts['fold000544'] = '<a href="javascript:fold(\'fold000544\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 544 to line 545</i>';
states['fold000544'] = false;
texts['fold000548'] = '<a href="javascript:fold(\'fold000548\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 548 to line 559</i>';
states['fold000548'] = false;
texts['fold000561'] = '<a href="javascript:fold(\'fold000561\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 561 to line 561</i>';
states['fold000561'] = false;
texts['fold000564'] = '<a href="javascript:fold(\'fold000564\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 564 to line 577</i>';
states['fold000564'] = false;
texts['fold000579'] = '<a href="javascript:fold(\'fold000579\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 579 to line 579</i>';
states['fold000579'] = false;
texts['fold000582'] = '<a href="javascript:fold(\'fold000582\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 582 to line 595</i>';
states['fold000582'] = false;
texts['fold000598'] = '<a href="javascript:fold(\'fold000598\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 598 to line 4662</i>';
states['fold000598'] = false;

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
