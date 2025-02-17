# Preparando o ambiente para usar a biblioteca _Gloss_
> Gloss é uma biblioteca que permite que criemos GUIs 2D

---

## Configure o arquivo `.cabal`

Dentro do arquivo do **`app.cabal`** procure o trecho:
```
executable haskell
    .
    .
    .
    build-depends:    base >=4.7 && <5, gloss >=1.13.2.2
    
    hs-source-dirs:   app
    
    default-language: Haskell2010
```

**É importante que `build-depends:    base >=4.7 && <5, gloss >=1.13.2.2` esteja configurado dessa forma.**


No terminal execute para finalizar a configuração:

**`cabal install gloss`**
**`cabal update`**
**`cabal build`**

Provavelmente, algum desses comandos **retornou uma mensagem de erro** sobre o _FreeGlut_ ou _OpenGL_ parecido com `unknown GLUT entry glutInit`.
Ajustaremos isso em sequência ;).

---

## Dependências
> No Windows, é necessário instalar mais algumas ferramentas para o funcionamento do app

#### Primeiramente, vamos garantir que o MSYS2 foi instalado pelo GHCup.

**No terminal, execute:**
> **`ghcup install msys2`**

**Se já estiver instalado deve mostrar uma mensagem do tipo:**
> **`[ Info ] MSYS2 is already installed at C:\Users\SeuUsuário\.ghcup\msys64`**

É provavel que **você tenha que adicionar ele ao _PATH_** para que possa utilizá-lo sem problemas.
No seu teclado, aperte **Windows+R**.
Na janela que surgiu, digite **`sysdm.cpl`** e clique em **OK**.
Na nova janela, vá na aba **Avançado** > **Váriaveis de Ambiente**.
Na nova janela, busque, no quadro "**Váriaveis do sistema**", busque pela váriavel **Path**.
Selecione ela, clicando sobre ela, e clique no botão **Editar**.
Na nova janela, clique em **NOVO** e cole o caminho para as pastas (crie um NOVO para cada um):

> **`C:\ghcup\bin`**
> **`C:\ghcup\msys64`**
> **`C:\ghcup\msys64\mingw64\bin`**

Confirme as alterações clicando em **OK** até que as janelas se fechem.

Pronto! Agora, reinicie o terminal do Windows e abra o **terminal do _MSYS2_** para finalizarmos, simplesmente executando **`msys2`**

No terminal do _MSYS2_, execute e aguarde a instalação: **`pacman -S mingw-w64-x86_64-freeglut`**

Agora tente compilar e executar o **`app`** novamente.
De volta ao terminal do Windows, execute:
> **`cabal build`**
> **`cabal run`**

É esperado que tudo funcione sem problemas agora.