# Branches no Git
> Passo a passo no terminal

Criar branches é utíl quando existem **muitas pessoas trabalhando no mesmo repositório.**

Elas ajudam a **separar o que cada um está fazendo sem que o que um alterar em um arquivo, quebre o que outro está fazendo no mesmo arquivo ou em outro.**

Elas criam uma "cópia" separada do repositório, em paralelo, as outras branches que possam existir.

**A branch `main` é a principal, onde devem estar apenas as partes finalizadas do trabalho.**

A seguir, vamos ver como manipular uma branch.

---
## Criando uma branch

### Importante!
Tente manter uma nomenclatura consistente para as branches. Siga o padrão para nomes do tipo **`<tipo>/<descrição>`** (ou ainda **`<tipo>/<nome>/<descrição>`**).

Tipos comuns:<br>
&nbsp;&nbsp;&nbsp;&nbsp;`feature`: para novas funcionalidades (Ex.: feature/HUB-inicial)<br>
&nbsp;&nbsp;&nbsp;&nbsp;`bugfix`: para correção de bugs (Ex.: bugfix/arthur/moverPeca-estranho)<br>
&nbsp;&nbsp;&nbsp;&nbsp;`docs`: para documentação (Ex.: docs/Update-readme)

Para a descrição, evite nomes genéricos, seja descritivo e consiso.

---

Para criar uma nova branch no **repositório local**, execute:<br>
> **`git branch <nome-branch>`**

Para mudar para qualquer branch criada, use:<br>
> **`git checkout <nome-branch>`**<br>

Se estiver no vscode, a branch atual aparece no canto inferior esquerdo.

> Você também pode usar **`git checkout -b <nome-branch>`** para criar a branch e mudar para ela.

---
## Listando branches

Para listas as branches **locais** use:<br>
> **`git branch`**

Caso queira listar as branches **locais** e **remotas** use:<br>
> **`git branch -a`**

---
## Merge entre branches

Ao finalizar (ou não), o que estava fazendo em sua branch, **você deve mesclar o que fez nela com o que existe na branch principal `main`.**
> **Atenção!** Ao tentar realizar um `merge` pode acontecer de existirem **arquivos conflitantes entre as branches.**<br>
> Para resolver isso, você deve **tentar o merge**, caso existam arquivos que conflitem, você deve **identifica-los e resolver as partes que colidem manualmente**.


Para mesclar as branches, execute:<br>
> **`git chekout main`** <span style="color: #888;">muda para a main</span><br>
> **`git merge <nome-branch>`** <span style="color: #888;">tenta mesclar as branches</span>

---
## Apagando branch

Para excluir uma branch, você pode optar por essas 2 alternativas:<br>
> **`git branch -d <nome-branch>`** <span style="color: #888;">exclui uma branch que já foi mesclada</span><br>
> **`git branch -D <nome-branch>`** <span style="color: #888;">força a exclusão de uma branch que ainda não foi mesclada</span>

---
## Branches locais e remotas

Existe a possibilidade de criar uma branch localmente e querer adicioná-la no repositório remoto, e vice-versa.

Para enviar uma **branch local** para o **repositório remoto**, use:<br>
> **`git push origin <nome-branch>`**

Para trazer uma **branch remota** para o **repositório local**, use:<br>
> **`git chekout --track origin/<nome-branch>`**

> Dica: use `git branch -a` para ver os nomes das branches remotas

> !!! BOA PRÁTICA !!! sempre antes de alterar arquivos e de realizar addes, commits ou pushes, use `git pull` para atualizar o repositório local. Ajuda a evitar conflitos entre arquivos.