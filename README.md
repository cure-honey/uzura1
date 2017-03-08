# uzura1
mpeg layer 1 encoder written in Fortran 2003/08  
#
uzura1 is rewritten in fortran 2003/08.
cheked with intel fortran ver.17.02 and gfortran 5.4.0

little endian is assumed.

compiler options
-for intel
 "/standard-semantics" is required for intel fortran.

-for gfortran
-std=f2008
gfortran -std=f2008  kind.f90 mpg.f90 mpg_io.f90 wav_io.f90 filter.f90 crc.f90 psycho.f90 layer1.f90 uzura1win.f90  


#
またリトル・エンディアンが仮定されています。

インテルの intel fortran ver.17.02 で実行確認しています。
コンパイラのオプションとして「F2003セマンティクスを有効にする」必要があります。そうしないと実行時エラーが発生します。

gfortran ver. 5.4.0 でも動作確認できました。