       identification division.
       program-id. LIBCON01 as "LIBCON01".

       environment division.
       file-control.
           select bblivros 
           assign to disk
           organization indexed
           access mode dynamic
           record key is tombo
           alternate record key is titulo
           alternate record key is autor
           alternate record key is editora
           file status is st.
       configuration section.
       special-names. decimal-point is comma.
       
       data division.
       file section.
           fd bblivros
           label record standard
           data record is livro
           value of file-id is "BBLIVROS.DAT".
       01 livro.
           02 tombo       pic 9(06).
           02 titulo      pic x(30).
           02 autor       pic x(20).
           02 editora     pic x(15).
           02 ano         pic 9999.
           02 doacao      pic a.
           02 doador      pic x(20).
           02 preco       pic 9(04)v99.
           02 procedencia pic x(20).
           02 emprestado  pic 9(05).
       working-storage section.
       01 st    pic xx.
       01 op    pic 9 value zero.
       01 valid pic aa value 'n'.
       01 sav   pic a.
       01 alt   pic a.
       01 del   pic a.
       01 con   pic a.
       01 cont  pic a.
       01 msg-blank pic a(60) value spaces.
       01 data-sis.
           02 ano-s pic 9999.
           02 mes   pic 99.
           02 dia   pic 99.
       01 livro-w.
           02 tombo-w       pic 9(06).
           02 titulo-w      pic x(30).
           02 autor-w       pic x(20).
           02 editora-w     pic x(15).
           02 ano-w         pic 9999.
           02 doacao-w      pic a.
           02 doador-w      pic x(20).
           02 preco-w       pic 9(04)v99.
           02 procedencia-w pic x(20).
           02 emprestado-w  pic 9(05) value zeros.
       01 tombo-e pic ZZZZZ9.    
       01 preco-e pic $.$$9,99.
       01 meses-ano.
           02 pic x(10) value "Janeiro".
           02 pic x(10) value "Fevereiro".
           02 pic x(10) value "Marco".
           02 pic x(10) value "Abril".
           02 pic x(10) value "Maio".
           02 pic x(10) value "Junho".
           02 pic x(10) value "Julho".
           02 pic x(10) value "Agosto".
           02 pic x(10) value "Setembro".
           02 pic x(10) value "Outubro".
           02 pic x(10) value "Novembro".
           02 pic x(10) value "Dezembro".
       01 tabela-meses redefines meses-ano.
           02 mes-t pic x(10) occurs 12 times.
       screen section.
       01 menu-inicial foreground-color 14.
           02 blank screen.
           02 line 01 col 01 value "Santos,    de            de     ".
           02 line 01 col 66 value "Biblioteca Central".
           02 line 02 col 33 value "       Controle de Livros".
           02 line 04 col 33 value "         Menu Principal" 
           foreground-color 11.
           02 line 06 col 33 value "       1 - Incluir Livros" 
           foreground-color 11.
           02 line 07 col 33 value "       2 - Alterar Livros" 
           foreground-color 11.
           02 line 08 col 33 value "       3 - Excluir Livros" 
           foreground-color 11.
           02 line 09 col 33 value "       4 - Consultar Livros" 
           foreground-color 11.
           02 line 10 col 33 value "       5 - Sair"
           foreground-color 11.
           02 line 12 col 33 value "       Digite uma opcao [ ]" 
           foreground-color 11.
       01 tela-inclusao foreground-color 14.
           02 blank screen.
           02 line 01 col 01 value "Santos,    de            de     ".
           02 line 01 col 66 value "Biblioteca Central".
           02 line 02 col 33 value "       Controle de Livros".
           02 line 04 col 33 value "       INCLUSAO DE LIVROS" 
           foreground-color 11.
           02 campos foreground-color 11.
               03 line 06 col 01 value "        TOMBO:".
               03 line 07 col 01 value "       TITULO:".
               03 line 08 col 01 value "        AUTOR:".
               03 line 09 col 01 value "      EDITORA:".
               03 line 10 col 01 value "          ANO:".
               03 line 11 col 01 value "DOACAO? (s/n):".
       01 tela-alteracao foreground-color 14.
           02 blank screen.
           02 line 01 col 01 value "Santos,    de            de     ".
           02 line 01 col 66 value "Biblioteca Central".
           02 line 02 col 33 value "       Controle de Livros".
           02 line 04 col 33 value "       ALTERACAO DE LIVRO" 
           foreground-color 11.
           02 campos foreground-color 11.
               03 line 06 col 01 value "        TOMBO:".
               03 line 07 col 01 value "       TITULO:".
               03 line 08 col 01 value "        AUTOR:".
               03 line 09 col 01 value "      EDITORA:".
               03 line 10 col 01 value "          ANO:".
               03 line 11 col 01 value "DOACAO? (s/n):".
       01 tela-exclusao foreground-color 14.
           02 blank screen.
           02 line 01 col 01 value "Santos,    de            de     ".
           02 line 01 col 66 value "Biblioteca Central".
           02 line 02 col 33 value "       Controle de Livros".
           02 line 04 col 33 value "       EXCLUSAO DE LIVRO" 
           foreground-color 11.
           02 line 06 col 01 value "        TOMBO:" foreground-color 11.
       01 tela-consulta foreground-color 14.
           02 blank screen.
           02 line 01 col 01 value "Santos,    de            de     ".
           02 line 01 col 66 value "Biblioteca Central".
           02 line 02 col 33 value "       Controle de Livros".
           02 campos foreground-color 11.
               03 line 04 col 33 value "       CONSULTA DE LIVROS".
               03 line 06 col 01 display 
               "Consultar por: [T]itulo ou t[O]mbo [ ]".
               03 line 08 col 01 value "        TOMBO:".
               03 line 09 col 01 value "       TITULO:".
               03 line 10 col 01 value "        AUTOR:".
               03 line 11 col 01 value "      EDITORA:".
               03 line 12 col 01 value "          ANO:".
               03 line 13 col 01 value "       DOACAO:".
       01 tela-encerra foreground-color 14.
           02 blank screen.
           02 line 05 col 33 value "FIM DE PROGRAMA".
           02 line 06 col 33 value space.
           
       procedure division.
       inicio.
           move function current-date to data-sis.
           perform abre-arquivo.
           perform menu-principal.
           perform recebe-opcao.
       abre-arquivo.
           open i-o bblivros.
           evaluate st
           when 05
           when 10
           when 00
           when 41
           when 42
               exit
           when other
               display erase at 0101
               display "ERRO DE ARQUIVO" at 0533 foreground-color 12
               stop run.
       menu-principal.
           display menu-inicial at 0101.
           perform exibe-data.
       exibe-data.
           display dia at 0109.
           display mes-t(mes) at 0115.
           display ano-s at 0129.
       recebe-opcao.
           accept op at 1258 with prompt auto.
           evaluate op
           when 1
               perform incluir-livro
           when 2
               perform alterar-livro
           when 3
               perform excluir-livro
           when 4
               perform consultar-livro
           when 5
               perform fim
           when other
               display "Opcao invalida. Verifique e redigite" at 1801 
               foreground-color 12
               perform recebe-opcao.
       
      *******************  INCLUIR NOVO LIVRO  *************************
       incluir-livro.
           initialize valid livro livro-w tombo-e.
           display tela-inclusao at 0101.
           perform exibe-data.
           perform valida-dados until valid = 'ok'.
           perform gravar-dados.
           perform continua.
       valida-dados.
           initialize valid.
           perform valida-tombo until valid = 's'.
           initialize valid.
           perform valida-titulo until valid = 's'.
           initialize valid.
           perform valida-autor until valid = 's'.
           initialize valid.
           perform valida-editora until valid = 's'.
           initialize valid.
           perform valida-ano until valid = 's'.
           initialize valid.
           perform valida-doacao until valid = 's'.
           evaluate doacao-w
           when 's'
               initialize valid
               perform valida-doador until valid = 's'
               move zeros to preco-w
               move spaces to procedencia-w
           when 'n'
               move spaces to doador-w
               initialize valid
               perform valida-preco until valid = 's'
               initialize valid
               perform valida-procedencia until valid = 's'.
           move 'ok' to valid.                                          
       valida-tombo.
           accept tombo-e at 0616 with prompt auto foreground-color 14.
           move tombo-e to tombo-w.
           evaluate tombo-w
           when zero
           when zeros
               display msg-blank at 1801
               display "Tombo invalido. Verifique e redigite" at 1801 
               foreground-color 12
           when other
               move tombo-w to tombo
               perform abre-arquivo
               read bblivros key is tombo
                   invalid key
                       display msg-blank at 1801
                       move 's' to valid
                       close bblivros
                   not invalid key
                       display msg-blank at 1801
                       display "Tombo ja cadastrado." at 1801 
                       foreground-color 12.
       valida-titulo.
           accept titulo-w at 0716 with prompt auto foreground-color 14.
           evaluate titulo-w
           when space
           when spaces
               display "Titulo invalido. Verifique e redigite" at 1801
               foreground-color 12
           when other
               display msg-blank at 1801
               move 's' to valid.
       valida-autor.
           accept autor-w at 0816 with prompt auto foreground-color 14. 
           evaluate autor-w
           when space
           when spaces
               display "Autor invalido. Verifique e redigite" at 1801 
               foreground-color 12
           when other
               display msg-blank at 1801
               move 's' to valid.
       valida-editora.
           accept editora-w at 0916 with prompt auto foreground-color 
           14.
           evaluate editora-w
           when space
           when spaces
               display "Editora invalida. Verifique e redigite" at 1801 
               foreground-color 12
           when other
               display msg-blank at 1801
               move 's' to valid.
       valida-ano.
           accept ano-w at 1016 with prompt auto foreground-color 14.   
           evaluate ano-w
           when zero
           when zeros
               display "Ano invalido. Verifique e redigite" at 1801 
               foreground-color 12
           when other
               if ano-w > ano-s                                         
                   display "Ano invalido. Maior que o atual, redigite" 
                   at 1801 foreground-color 12
               else
                   display msg-blank at 1801
                   move 's' to valid.
       valida-doacao.
           accept doacao-w at 1116 with prompt auto foreground-color 14.
           evaluate doacao-w
           when 's'
           when 'n'
               display msg-blank at 1801
               move 's' to valid
           when other
               display "Opcao invalida, somente 's' ou 'n'" at 1801 
               foreground-color 12.
       valida-doador.
           display "       DOADOR:" at 1201 foreground-color 11.
           accept doador-w at 1216 with prompt auto foreground-color 14.
           evaluate doador-w
           when space
           when spaces
           when zero
           when zeros
               display "Doador invalido. Verifique e redigite" at 1801 
               foreground-color 12
           when other
               display msg-blank at 1801
               move 's' to valid.
       valida-preco.
           display "        PRECO:" at 1201 foreground-color 11.
           accept preco-e at 1216 with prompt auto foreground-color 14.
           move preco-e to preco-w.
           evaluate preco-w
           when zero
           when zeros
               display "Preco invalido. Verifique e redigite" at 1801 
               foreground-color 12
           when other
               display msg-blank at 1801
               move 's' to valid.
       valida-procedencia.
           display "  PROCEDENCIA:" at 1301 foreground-color 11.
           accept procedencia-w at 1316 with prompt auto 
           foreground-color 14.
           evaluate procedencia-w
           when space
           when spaces
               display "Procedencia invalida. Verifique e redigite" at 
               1801 foreground-color 12
           when other
               display msg-blank at 1801
               move 's' to valid.
       gravar-dados.
           display msg-blank at 1801.
           display "Deseja gravar o novo livro? (s/n) [ ]" at 1801 
           foreground-color 10.
           accept sav at 1836 with prompt auto.
           evaluate sav
           when 's'
               perform grava-livro
           when 'n'
               perform continua
           when other
               perform gravar-dados.
       grava-livro.
           move livro-w to livro.
           perform abre-arquivo.
           write livro.
           evaluate st
           when "00"
               close bblivros
           when other
               display msg-blank at 1801
               display "ERRO DE GRAVACAO!" at 1801 foreground-color 12
               stop run.
       continua.
           display msg-blank at 1801.
           display "Deseja incluir outro livro? (s/n) [ ]" at 1801 
           foreground-color 10.
           accept cont at 1836 with prompt auto.
           evaluate cont
           when 's'
               perform incluir-livro
           when 'n'
               perform inicio
           when other
               display msg-blank at 1801
               display "Opcao invalida. Verifique e redigite" at 1801 
               foreground-color 12.
   
      ************************  ALTERAR LIVRO  *************************
       alterar-livro.
           initialize livro livro-w valid cont sav.
           display tela-alteracao at 0101.
           perform exibe-data.
           perform buscar-livro until valid = 's'.
           perform exibe-livro.
           initialize valid.
           perform aceita-alterar until valid = 'ok'.
           perform regravar-livro.
           perform continua-alt.
       buscar-livro.
           perform abre-arquivo.
           accept tombo-e at 0616 with prompt auto foreground-color 14.
           move tombo-e to tombo-w.
           evaluate tombo-w
           when zero
           when zeros
               display msg-blank at 1801
               display "Tombo invalido. Verifique e redigite" at 1801 
               foreground-color 12
           when other
               move tombo-w to tombo
               read bblivros key is tombo
                   invalid key
                       display msg-blank at 1801
                       display "Livro nao cadastrado" at 1801 with 
                       foreground-color 12
                   not invalid key
                       display msg-blank at 1801
                       move 's' to valid.
       exibe-livro.
           move livro to livro-w.
           move preco-w to preco-e.
           display tombo-e   at 0616 with foreground-color 10.
           display titulo-w  at 0716 with foreground-color 10.
           display autor-w   at 0816 with foreground-color 10.
           display editora-w at 0916 with foreground-color 10.
           display ano-w     at 1016 with foreground-color 10.
           display doacao-w  at 1116 with foreground-color 10.
           evaluate doacao-w
           when 's'
           when 'S'
               display "       DOADOR:" at 1201 with foreground-color 
               11
               display doador-w at 1216 with foreground-color 10
           when 'n'
           when 'N'
               display "        PRECO:" at 1201 with foreground-color 
               11
               display preco-e at 1216 with foreground-color 10
               display "  PROCEDENCIA:" at 1301 with foreground-color 
               11
               display procedencia-w at 1316 with foreground-color 10.  
       aceita-alterar.
           display msg-blank at 1801.
           display "Deseja alterar o livro selecionado? (s/n) [ ]" at 
           1801 with foreground-color 10.
           accept alt at 1844 with prompt auto.
           evaluate alt
           when 's'
           when 'S'
               display msg-blank at 1801
               perform altera-livro
           when 'n'
           when 'N'
               display msg-blank at 1801
               display "Deseja buscar outro livro? (s/n) [ ]" at 1801 
               with foreground-color 10
               accept cont at 1835 with prompt auto
               evaluate cont
               when 's'
               when 'S'
                   perform alterar-livro
               when 'n'
               when 'N'
                   perform inicio.
       altera-livro.
           perform valida-titulo until valid = 's'.
           initialize valid.
           perform valida-autor until valid = 's'.
           initialize valid.
           perform valida-editora until valid = 's'.
           initialize valid.
           perform valida-ano until valid = 's'.
           initialize valid.
           perform valida-doacao until valid = 's'.
           evaluate doacao-w
           when 's'
           when 'S'
               display " " at 1201 erase eos
               initialize valid
               perform valida-doador until valid = 's'
               move zeros to preco-w
               move spaces to procedencia-w
           when 'n'
           when 'N'
               display " " at 1201 erase eos
               move spaces to doador-w
               initialize valid
               perform valida-preco until valid = 's'
               initialize valid
               perform valida-procedencia until valid = 's'.
           move 'ok' to valid.
       regravar-livro.
           display msg-blank at 1801.
           display "Deseja regravar os dados do livro? (s/n) [ ]" at 
           1801 with foreground-color 10.
           accept sav at 1843 with prompt auto.
           evaluate sav
           when 's'
           when 'S'
               perform regrava-livro
           when 'n'
           when 'N'
               perform continua-alt
           when other
               perform regravar-livro.
       regrava-livro.
           move livro-w to livro.
           perform abre-arquivo.
           rewrite livro.
           evaluate st
           when "00"
               close bblivros
           when other
               display msg-blank at 1801
               display "ERRO DE GRAVACAO!" at 1801 foreground-color 12
               stop run.
       continua-alt.
           initialize cont.
           display msg-blank at 1801.
           display "Deseja alterar outro livro? (s/n) [ ]" at 1801
           foreground-color 10.
           accept cont at 1836 with prompt auto.
           evaluate cont
           when 's'
           when 'S'
               perform alterar-livro
           when 'n'
           when 'N'
               perform inicio
           when other
               display msg-blank at 1801
               display "Opcao invalida. Verifique e redigite" at 1801 
               foreground-color 12.
       
      ************************  EXCLUIR LIVRO  *************************
       excluir-livro.
           initialize livro livro-w del cont valid.
           display tela-exclusao at 0101.
           perform exibe-data.
           perform buscar-livro until valid = 's'.
           display "        TOMBO:" at 0601 with foreground-color 11.
           display "       TITULO:" at 0701 with foreground-color 11.
           display "        AUTOR:" at 0801 with foreground-color 11.
           display "      EDITORA:" at 0901 with foreground-color 11.
           display "          ANO:" at 1001 with foreground-color 11.
           display "       DOACAO:" at 1101 with foreground-color 11.
           perform exibe-livro.
           perform aceita-excluir.
       aceita-excluir.
           display msg-blank at 1801.
           display "Deseja excluir o livro selecionado? (s/n) [ ]" at 
           1801 with foreground-color 10.
           accept del at 1844 with prompt auto.
           evaluate del
           when 's'
           when 'S'
               perform exclui-livro
           when 'n'
           when 'N'
               perform continua-del
           when other
               perform aceita-excluir.
       exclui-livro.
           delete bblivros.
           evaluate st
           when 00
               close bblivros
               perform continua-del
           when other
               display msg-blank at 1801
               display "ERRO DE EXCLUSAO!" at 1801 foreground-color 12
               stop run.
       continua-del.
           initialize cont.
           display msg-blank at 1801.
           display "Deseja excluir outro livro? (s/n) [ ]" at 1801
           foreground-color 10.
           accept cont at 1836 with prompt auto.
           evaluate cont
           when 's'
           when 'S'
               perform excluir-livro
           when 'n'
           when 'N'
               perform inicio
           when other
               display msg-blank at 1801
               display "Opcao invalida. Verifique e redigite" at 1801 
               foreground-color 12.

      *********************  CONSULTAR LIVROS  *************************
       consultar-livro.
           initialize livro livro-w cont con valid.
           display tela-consulta at 0101.
           perform exibe-data.
           perform tipo-busca.
       tipo-busca.
           accept con at 0637 with prompt auto foreground-color 14.
           evaluate con
           when 't'
           when 'T'
               perform busca-titulo
           when 'o'
           when 'O'
               perform busca-tombo
           when other
               display msg-blank at 1801
               display "Opcao invalida. Verifique e redigite" at 1801
               foreground-color 12
               perform tipo-busca.
       busca-titulo.
           display "- Titulo:" at 0640 with foreground-color 11.
           accept titulo-w at 0650 with prompt auto foreground-color 14.
           evaluate titulo-w
           when space
           when spaces
               display msg-blank at 1801
               display "Opcao invalida. Verifique e redigite" at 1801
               foreground-color 12
               perform busca-titulo
           when other
               move titulo-w to titulo
               perform abre-arquivo
               read bblivros key is titulo
                   invalid key perform nao-encontrado
                   not invalid key perform encontrado.
       busca-tombo.
           display "- Tombo:" at 0640 with foreground-color 11.
           accept tombo-e at 0649 with prompt auto foreground-color 14.
           move tombo-e to tombo-w.
           evaluate tombo-w
           when zero
           when zeros
               display msg-blank at 1801
               display "Opcao invalida. Verifique e redigite" at 1801
               foreground-color 12
               perform busca-tombo
           when other
               move tombo-w to tombo
               perform abre-arquivo
               read bblivros key is tombo
                   invalid key perform nao-encontrado
                   not invalid key perform encontrado.
       nao-encontrado.
           display msg-blank at 1701.
           display "Livro nao encontrado" at 
           1701 foreground-color 12.
           perform continua-con.
       encontrado.
           move livro to livro-w.
           move preco-w to preco-e.
           display tombo-e   at 0816 with foreground-color 10.
           display titulo-w  at 0916 with foreground-color 10.
           display autor-w   at 1016 with foreground-color 10.
           display editora-w at 1116 with foreground-color 10.
           display ano-w     at 1216 with foreground-color 10.
           display doacao-w  at 1316 with foreground-color 10.
           evaluate doacao-w
           when 's'
           when 'S'
               display "       DOADOR:" at 1401 with foreground-color 
               11
               display doador-w at 1416 with foreground-color 10
           when 'n'
           when 'N'
               display "        PRECO:" at 1401 with foreground-color 
               11
               display preco-e at 1416 with foreground-color 10
               display "  PROCEDENCIA:" at 1501 with foreground-color 
               11
               display procedencia-w at 1516 with foreground-color 10.
           perform continua-con.
       continua-con.
           initialize cont.
           display msg-blank at 1801.
           display "Deseja buscar outro livro? (s/n) [ ]" at 1801
           foreground-color 10.
           accept cont at 1835 with prompt auto.
           evaluate cont
           when 's'
           when 'S'
               perform consultar-livro
           when 'n'
           when 'N'
               perform inicio
           when other
               display msg-blank at 1801
               display "Opcao invalida. Verifique e redigite" at 1801 
               foreground-color 12.
       fim.
           display tela-encerra at 0101.
           stop run.

       end program LIBCON01.
