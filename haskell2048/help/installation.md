# Guia de instalação
> Instalando o GHCup (gerenciador de downloads para Haskell e dependências) no Windows
---
## Instale o GHCup:
### Execute no terminal:
**`Set-ExecutionPolicy Bypass -Scope Process -Force;[System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072; try { & ([ScriptBlock]::Create((Invoke-WebRequest https://www.haskell.org/ghcup/sh/bootstrap-haskell.ps1 -UseBasicParsing))) -Interactive -DisableCurl } catch { Write-Error $_ }`**

Após a intalação, reinicie o terminal e execute `ghcup --version` para testar a instalação.

> **Verifique se o _GHC_ foi instaldo corretamente com `ghc --version`**<br>
> **Verifique se o _Cabal_ foi instaldo corretamente com `cabal --version`**
---
### Instale o GHC e o Cabal (caso necessário):
### Execute no terminal:
> **`ghcup install ghc latest`**<br>
> **`ghcup set ghc latest`**
>
> **`ghcup install cabal 3.12.1.0`**
> **`ghcup set cabal 3.12.1.0`**<br>

Tente executar novamente os comandos para verificar se os downloads foram executados corretamente.
