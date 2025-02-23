# Preparando o ambiente para usar a biblioteca _Gloss_
> Gloss é uma biblioteca que permite que criemos GUIs 2D

---
> Execute os comandos dentro da pasta geral do projeto **`haskell2048`**
## Configure o arquivo `.cabal`

Dentro do arquivo do **`haskell2048.cabal`** procure o trecho:
```
executable haskell2048
    .
    .
    .
    build-depends:    base >=4.7 && <5, gloss >=1.13.2.2
    
    hs-source-dirs:   app
    
    default-language: Haskell2010
```

**É importante que `build-depends:    base >=4.7 && <5, gloss >=1.13.2.2` esteja configurado dessa forma.**


No terminal execute para finalizar a configuração:

**`cabal install gloss`**<br>
**`cabal update`**<br>
**`cabal build`**

Provavelmente, algum desses comandos **retornou uma mensagem de erro** sobre o _FreeGlut_ ou _OpenGL_, parecido com:
> `unknown GLUT entry glutInit`.

Ajustaremos isso em sequência ;).

---

## Dependências
> No Windows, é necessário instalar mais algumas ferramentas para o funcionamento do app

#### Primeiramente, vamos garantir que o MSYS2 foi instalado pelo GHCup.

**No terminal, execute:**
> **`dir C:\ghcup\msys64`**

**Se já estiver instalado deve mostrar os arquivos da pasta.**

É provavel que **você tenha que adiciona-lo ao _PATH_** para que possa utilizá-lo sem problemas.
No seu teclado, aperte **Windows+R**.
Na janela que surgiu, digite **`sysdm.cpl`** e clique em **OK**.
Na nova janela, vá na aba **Avançado** > **Váriaveis de Ambiente**.
Na nova janela, busque, no quadro "**Váriaveis do sistema**", busque pela váriavel **Path**.
Selecione ela, clicando sobre ela, e clique no botão **Editar**.
Na nova janela, clique em **NOVO** e cole o caminho para as pastas (crie um NOVO para cada um):

> **`C:\ghcup\bin`**<br>
> **`C:\ghcup\msys64`**<br>
> **`C:\ghcup\msys64\mingw64\bin`**

Confirme as alterações clicando em **OK** até que as janelas se fechem.

Pronto! Agora, reinicie o terminal do Windows e abra o **terminal do _MSYS2_** para finalizarmos, simplesmente executando **`msys2`**

No terminal do _MSYS2_, execute: **`$ pacman -Syu`** e depois
**`pacman -S mingw-w64-x86_64-freeglut`** e aguarde instalação.

Agora tente compilar e executar o **`app`** novamente.
De volta ao terminal do Windows, execute:
> **`cabal build`**<br>
> **`cabal run`**

É esperado que tudo funcione sem problemas agora.
