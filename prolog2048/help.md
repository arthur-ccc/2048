# Instalação SWI-Prolog
> Passo a passo no terminal

Aqui vou mostrar como fiz no **Linux**, que é o SO recomendado para maior compatibilidade.

## No terminal

### Instalando as depêndencias...

No terminal:

> **`sudo apt update`**

> **`sudo apt install -y \`**<br>
> &nbsp;&nbsp;&nbsp;&nbsp;**`git build-essential cmake \`**<br>
> &nbsp;&nbsp;&nbsp;&nbsp;**`libxext-dev libxft-dev libxpm-dev libjpeg-dev \`**<br>
> &nbsp;&nbsp;&nbsp;&nbsp;**`libgmp-dev libedit-dev \`**<br>
> &nbsp;&nbsp;&nbsp;&nbsp;**`libssl-dev unixodbc-dev \`**<br>
> &nbsp;&nbsp;&nbsp;&nbsp;**`zlib1g-dev pkg-config \`**<br>
> &nbsp;&nbsp;&nbsp;&nbsp;**`libreadline-dev libpcre2-dev \`**<br>
> &nbsp;&nbsp;&nbsp;&nbsp;**`libdb-dev libncurses-dev \`**<br>
> &nbsp;&nbsp;&nbsp;&nbsp;**`libffi-dev libxinerama-dev libxrandr-dev \`**<br>
> &nbsp;&nbsp;&nbsp;&nbsp;**`libgl1-mesa-dev libglu1-mesa-dev \`**<br>
> &nbsp;&nbsp;&nbsp;&nbsp;**`libgtk-3-dev`**

---

### Agora vamos clonar o repositório oficial do SWI_Prolog:


#### Antes!

Pode ser necessário aumentar o tamanho do buffer do git:

> **`git config --global http.postBuffer 524288000`**

#### Agora: 

> **`git clone https://github.com/SWI-Prolog/swipl-devel.git`**<br>
> **`cd swipl-devel`**<br>
> **`git submodule update --init`**

---

### Hora de compilar e instalar:

> **`mkdir build`**<br>
> **`cd build`**<br>
> **`cmake -DCMAKE_INSTALL_PREFIX=/usr/local ..`**<br>
> **`make -j$(nproc)`**<br>
> **`sudo make install`**

---

### Vamos testar a intalação:

Abra o prolog:

> **`swipl`**

Agora tente carregar a interface gráfica:

> **`use_module(library(pce)).`**

O esperado é que **carregue sem erro**, o que significa que a `XPCE` foi instalada com sucesso!