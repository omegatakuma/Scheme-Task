SchemeToDo
===========
ToDoを管理するスクリプト

使い方
------

ToDo追加
-------
`./task.scm -w`
`:w`

	?> :w
	NewTask> Haskeる
	Date> 2012/5/1
	[1]|2012/5/1|: Haskeる
	-> 残り40日です。

+ 残り日数が表示される。
+ 日付が過ぎている場合は **期限が過ぎていきます** と表示される。
+ 日付が当時の場合は **期限日です。** と表示される。

ToDo一覧
-------
`./task.scm -r`
`:r`

	?> :r
	[1]|2012/5/1|: Haskeる
	-> 残り40日です。
	[2]|2012/5/2|: Schemeる
	-> 残り41日です。

+ ない場合は **Nothing** が表示される

ToDo削除
-------
`./task.scm -d`
`:del`

	?> :del
	> 1
	[2]|2012/5/2|: Schemeる
	-> 残り41日です。

+ `>`で指定した番号のタスクが削除される。
+ `>`でallを指定すると全て削除される。

ToDoの終了
---------
`:q`

+ **Vim**を意識したわけないはず

日付について
------------
`Date>`のフォーマットは`Y/M/D`です。

直すところ
----------
+ wを引き続きできるようにしたいが未だ...

バージョン
--------
### Ver 1.0 ###
バージョンを設定したほうがなんかソレっぽいので
**1.0** としよう！

### Ver 1.1 ###
バージョンを設定したほうがソレっぽいので
delが使えるような気がするこれを **1.1** としよう！

+ delを直した

### Ver 1.2 ###
+ 番号を直した

### Ver 1.3 ###
+ 番号が直せていなかったので直した
+ 番号を調整したことでいろいろ不具合があったので直した
+ [(1)]と表示されたので直した
+ 指定したコマンドが存在しない場合**command found not :[コマンド]**と表示する。

### Ver 1.4 ###
+ `:del`ですべて消した時に()だけ残っても**Nothing!!!!!**が表示されるように
+ なんとなく`>>>`から`?>`に変更。
+ `:w`で新規にTaskを追加する時に`:q`を入力することで`?>`に戻る。

### Ver 1.5 ###
+ `>`で`all`を指定すると全て削除する。
+ `:del`で削除したあとの順番がおかしかったので直した

### Ver 1.6 ###
+ `./task.scm`に`-w|write`、`-r|read`、`-d|del`が使えるようにした
