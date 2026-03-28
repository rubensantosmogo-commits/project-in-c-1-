/* iaed26 - ist1118472 - project */
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#define MAX_CARATERES 65535

typedef struct{
    char *ean;
    double preco;
    char iva;
    int stock;
    int dentro_cesto;
    char *descricao;
    int vendido;
} Prod;

typedef struct{
    Prod **prods;
    int size;
    int capacidade_max;
} Lista_produtos;

typedef struct{
    Prod *produto;
    int quantidade;
} Prods_cesto;

typedef struct{
    Prods_cesto *items;
    int size;
    int capacidade_max;
} Cesto;

typedef struct{
    int numero;
    int nif;
    char *cliente;
    int total_prod;
    double valor_total;
} Fatura;

typedef struct{
    Fatura **faturas;
    int size;
    int capacidade_max;
} Lista_faturas;

typedef struct{
    Lista_faturas faturas;
    Lista_produtos produtos;
    Cesto cesto;
    int iva[26];
    int iva_usado[26];
    int total_faturas;
    int faturas_ordenadas;
} Sistema;

void inicializar_iva(Sistema *s){
    int i;
    for(i = 0; i < 26; i++){
        s->iva[i] = 0;
        s->iva_usado[i] = 0;
    }
    s->iva['A'-'A'] = 0;
    s->iva['B'-'A'] = 6;
    s->iva['C'-'A'] = 13;
    s->iva['D'-'A'] = 23;
}

void organizar_faturas(Fatura **arr, int n, int i) {
    int largest = i; 
    int l = 2 * i + 1; 
    int r = 2 * i + 2; 
    if (l < n) {
        int cmp = strcmp(arr[l]->cliente, arr[largest]->cliente);
        if (cmp > 0 || (cmp == 0 && arr[l]->numero > arr[largest]->numero))
            largest = l;
    }
    if (r < n) {
        int cmp = strcmp(arr[r]->cliente, arr[largest]->cliente);
        if (cmp > 0 || (cmp == 0 && arr[r]->numero > arr[largest]->numero))
            largest = r;
    }
    if (largest != i) {
        Fatura *tmp = arr[i];
        arr[i] = arr[largest];
        arr[largest] = tmp;
        organizar_faturas(arr, n, largest);
    }
}

void sort_faturas(Fatura **arr, int n) {
    for (int i = n / 2 - 1; i >= 0; i--)
        organizar_faturas(arr, n, i);
    for (int i = n - 1; i > 0; i--) {
        Fatura *tmp = arr[0]; 
        arr[0] = arr[i];
        arr[i] = tmp;
        organizar_faturas(arr, i, 0);
    }
}

void ler_iva(Sistema *s, const char *nome){
    FILE *f = fopen(nome,"r");
    if(!f)
        return;
    char letra;
    int valor;
    while(fscanf(f," %c %d",&letra,&valor) == 2){
    if(letra >= 'A' && letra <= 'Z'){
        s->iva[letra-'A'] = valor;
        s->iva_usado[letra-'A'] = 1;
        }
    }
    fclose(f);
}

double arredondamento(double valor){
    if(valor >= 0)
        return ((long)(valor * 100 + 0.5 + 1e-9)) / 100.0;
    else
        return ((long)(valor * 100 - 0.5 - 1e-9)) / 100.0;
}

double calcular_iva(Sistema *s, double preco, char iva){
    int taxa = s->iva[iva-'A'];
    return preco * (1 + taxa/100.0);
}

int wildcard_match(const char *p, const char *t){
    const char *estrela = NULL, *backup = NULL;
    while(*t){
        if(*p == *t || *p == '?'){
            p++; t++;
        }
        else if(*p == '*'){
            estrela = p++;
            backup = t;
        }
        else if(estrela){
            p = estrela + 1;
            t = ++backup;
        }
        else{
            return 0;
        }
    }
    while(*p == '*') p++;
    return *p == '\0';
}

int validar_ean(const char *ean){
    int len = strlen(ean);
    int i, soma = 0, d, digito;
    if (len != 8 && len != 13)
        return 0;
    for(i = 0; i< len; i++){
        if(!isdigit((unsigned char)ean[i]))
            return 0;
    }
    if (len == 13){
        for (i = 0; i < 12; i++){
            d = ean[i] - '0';
            if (i % 2 == 0)
                soma += d;
            else
                soma += 3 * d;
        }
    }else {
        for (i = 0; i < 7; i++){
            d = ean[i] - '0';
            if (i % 2 == 0)
                soma +=d;
            else
                soma += 3 * d;
        }
    }
    digito = (10-(soma % 10)) % 10;
    return digito == (ean[len -1] - '0');
}

char *copiar_string(const char *s){
     char *nova = malloc(strlen(s) + 1);
    if(!nova){
        printf("No memory.\n");
        exit(0);
    }
    strcpy(nova, s);
    return nova;
}

char *ler_descricao(char *p){
    while (*p && isspace((unsigned char)*p))
        p++;
    if (*p == '\0')
        return NULL;
    size_t len = strlen(p);
    if (len > 0 && p[len - 1] == '\n')
        p[len -1] = '\0';
    char *descricao1 = copiar_string(p);
    return descricao1;
}

int validar_descricao(const char *descricao1){
    if(strlen(descricao1) > 50)
        return 0;
    if(descricao1[0] == '\0')
        return 0;
    return 1;
}

int validar_nome_cliente(const char *cliente){
    int i;
    if(!isalnum((unsigned char)cliente[0]))
        return 0;
    for(i = 0; cliente[i]; i++){
        if(!isalnum((unsigned char)cliente[i]) && !isspace((unsigned char)cliente[i]))
            return 0;
    }
    return 1;
}
int validar_nif(const int nif){
    return (nif >= 100000000 && nif <= 999999999);
}

Prod *procurar_prod(Lista_produtos *lp, const char *ean) {
    int i;
    for (i = 0; i < lp->size; i++) {
        if (lp->prods[i]->ean[0] == ean[0] && strcmp(lp->prods[i]->ean, ean) == 0)
            return lp->prods[i];
    }
    return NULL;
}


void adicionar_prod(Lista_produtos *lp, Prod *p){
    if(lp->size == lp->capacidade_max){
    int nova = lp->capacidade_max == 0 ? 10 : lp->capacidade_max * 2;
        Prod **novo = realloc(lp->prods, nova * sizeof(Prod*));
        if(!novo){
            printf("No memory.\n");
            exit(0);
        }
        lp->prods = novo;
        lp->capacidade_max = nova;
    }
    lp->prods[lp->size++] = p;
}

Prod *criar_prod(char *ean, char iva, double preco, int stock, char *descricao){
    Prod *p = malloc(sizeof(Prod));
    if(!p){
        printf("No memory.\n");
        exit(0);
    }
    p->ean = copiar_string(ean);
    p->descricao = copiar_string(descricao);
    p->iva = iva;
    p->preco = arredondamento(preco);
    p->stock = stock;
    p->vendido = 0;
    p->dentro_cesto = 0;
    return p;
}

void adicionar_fatura(Lista_faturas *lf, Fatura *f){
    if(lf->size == lf->capacidade_max){
    int nova = lf->capacidade_max == 0 ? 10 : lf->capacidade_max * 2;
        Fatura **novo = realloc(lf->faturas, nova * sizeof(Fatura*));
        if(!novo){
            printf("No memory.\n");
            exit(0);
        }
        lf->faturas = novo;
        lf->capacidade_max = nova;
    }
    lf->faturas[lf->size++] = f;
}

int introduzir_prod(Sistema *s, char *args){
    char ean[20];
    char iva;
    float preco;
    int stock;
    if(sscanf(args,"%19s %c %f %d", ean, &iva, &preco, &stock) != 4)
        return 0;
    if(!validar_ean(ean)){
        printf("invalid ean\n");
        return 0;
    }
    if(iva < 'A' || iva > 'Z' || !s->iva_usado[iva - 'A']){
        printf("invalid iva\n");
        return 0;
    }
    if(preco <= 0){
        printf("invalid price\n");
        return 0;
    }
    if(stock < 0){
        printf("invalid quantity\n");
        return 0;
    } 
    char buffer[100];
    if (sscanf(args,"%19s %c %f %d %[^\n]", ean, &iva, &preco, &stock, buffer) < 5) {
        printf("invalid description\n");
        return 0;
    }
    char *descricao = ler_descricao(buffer);
    if(!descricao || !validar_descricao(descricao)){
        printf("invalid description\n");
        free(descricao);
        return 0;
    }
    
    Prod *prod = procurar_prod(&s->produtos, ean);
    if(!prod && s->produtos.size >= 10000){
        printf("invalid product\n");
        free(descricao);
        return 0;
    }
    if(prod){
        if(prod->dentro_cesto){
            printf("product in use\n");
            free(descricao);
            return 0;
        }
        prod->iva = iva;
        prod->preco = arredondamento(preco);
        prod->stock += stock;
        free(prod->descricao);
        prod->descricao = copiar_string(descricao);
        printf("%d\n", prod->stock);
    } else{
        Prod *novo = criar_prod(ean, iva, preco, stock, descricao);
        printf("%d\n", stock);
        adicionar_prod(&s->produtos, novo);
    }
    free(descricao);
    return 0;
}

int listar_prod(Sistema *s, char *args) {
    int i, j;
    while (*args && isspace((unsigned char)*args)) args++;
    if (*args == '\0' || (args[0] == '*' && (args[1] == '\0' || isspace((unsigned char)args[1])))) {
        int encontrou = 0;
        for (i = 0; i < s->produtos.size; i++) {
            Prod *p = s->produtos.prods[i];
            if (p->stock > 0) {
                printf("%s %c %.2f %d %d %s\n",
                    p->ean, p->iva, p->preco, p->vendido, p->stock, p->descricao);
                encontrou = 1;
            }
        }
        if (!encontrou) {
            printf("*: no such product\n");
        }
        return 0;
    }
    char *tokens[100];
    int num_tokens = 0;
    char *t = strtok(args, " \t\n\r");
    while (t && num_tokens < 100) {
        tokens[num_tokens++] = t;
        t = strtok(NULL, " \t\n\r");
    }
    int *impresso = calloc(s->produtos.size, sizeof(int));
    for (i = 0; i < num_tokens; i++) {
        int encontrou_pelo_menos_um = 0;
        for (j = 0; j < s->produtos.size; j++) {
            Prod *p = s->produtos.prods[j];
            if (p->stock > 0 && wildcard_match(tokens[i], p->ean)) {
                if (!impresso[j]) {
                    printf("%s %c %.2f %d %d %s\n", 
                           p->ean, p->iva, p->preco, p->vendido, p->stock, p->descricao);
                    impresso[j] = 1; 
                }
                encontrou_pelo_menos_um = 1;
            }
        }
        if (!encontrou_pelo_menos_um) {
            printf("%s: no such product\n", tokens[i]);
        }
    }
    free(impresso);
    return 0;
}

void listar_cesto(Sistema *s){
    int i, j;
    for (i = 1; i < s->cesto.size; i++) {
        Prods_cesto key = s->cesto.items[i];
        j = i - 1;
        while (j >= 0 && strcmp(s->cesto.items[j].produto->ean,key.produto->ean) > 0) {
            s->cesto.items[j + 1] = s->cesto.items[j];
            j--;
        }
        s->cesto.items[j + 1] = key;
    }
         for(i = 0; i < s->cesto.size; i++){
            Prod *pr = s->cesto.items[i].produto;
            int q = s->cesto.items[i].quantidade;
            double total = arredondamento(calcular_iva(s, pr->preco, pr->iva) * q);
            if(q > 0){
                printf("%c %.2f %d %.2f %s\n",
                    pr->iva,
                    pr->preco,
                    q,
                    total,
                    pr->descricao);
            }
        }
}

int encontrar_posicao(Cesto *c, const char *ean){
    int i;
    for(i = 0; i < c->size; i++){
        if(strcmp(c->items[i].produto->ean, ean) == 0)
            return i;
    }
    return -1;
}

int adicionar_item(Sistema *s, Prod *p, int quantidade, int pos){
    if(p->stock < quantidade){
        printf("no stock\n");
        return -1;
    }
    if(pos == -1){
        if(s->cesto.size == s->cesto.capacidade_max){
            int nova = s->cesto.capacidade_max == 0 ? 10 : s->cesto.capacidade_max * 2;
            Prods_cesto *novo = realloc(s->cesto.items, nova * sizeof(Prods_cesto));
            if(!novo){
                printf("No memory.\n");
                exit(0);
            }
            s->cesto.items = novo;
            s->cesto.capacidade_max = nova;
        }
        s->cesto.items[s->cesto.size].produto = p;
        s->cesto.items[s->cesto.size].quantidade = quantidade;
        pos = s->cesto.size;
        s->cesto.size++;
    } else{
        s->cesto.items[pos].quantidade += quantidade;
    }
    p->stock -= quantidade;
    p->vendido += quantidade;
    p->dentro_cesto = 1;
    return pos;
}

int remover_item(Sistema *s, Prod *p, int quantidade, int pos){
    int i;
    if(pos == -1 || s->cesto.items[pos].quantidade < quantidade){
        printf("invalid quantity\n");
        return -1;
    }
    s->cesto.items[pos].quantidade -= quantidade;
    p->stock += quantidade;
    p->vendido -= quantidade;
    if(s->cesto.items[pos].quantidade == 0){
        for(i = pos; i < s->cesto.size - 1; i++)
            s->cesto.items[i] = s->cesto.items[i+1];
        s->cesto.size--;
        p->dentro_cesto = 0;
        printf("%c %.2f %d %.2f %s\n",
            p->iva, p->preco, 0, 0.0, p->descricao);
        return -1;
    }

    return pos;
}

int adicionar_cesto(Sistema *s, char *args){
    char ean[20];
    int quantidade, pos;
    Prod *p;
    if(args[0] == '\0'){
        listar_cesto(s);
        return 0;
    }
    if(sscanf(args,"%d %s",&quantidade,ean) != 2){
        quantidade = 1;
        sscanf(args,"%s",ean);
    }
    if(!validar_ean(ean)){
        printf("invalid ean\n");
        return 0;
    }
    p = procurar_prod(&s->produtos, ean);
    if(!p){
        printf("%s: no such product\n", ean);
        return 0;
    }
    pos = encontrar_posicao(&s->cesto, ean);
    if(quantidade > 0){
        pos = adicionar_item(s, p, quantidade, pos);
        if(pos == -1) return 0;
    }
    else{
        pos = remover_item(s, p, -quantidade, pos);
        if(pos == -1) return 0;  
    }
    if(pos != -1){
        int q = s->cesto.items[pos].quantidade;
        double total = arredondamento(calcular_iva(s, p->preco, p->iva) * q);
        printf("%c %.2f %d %.2f %s\n",
            p->iva, p->preco, q, total, p->descricao);
    }
    return 0;
}

int resumo_faturacao(Sistema *s, char *args){
    int i;
    int total_items = 0;
    double total_valor = 0;
    while(*args && isspace((unsigned char)*args))args++;
    if(*args == '\0'){
        for(i = 0 ; i<s->faturas.size ; i++){
            total_items += s->faturas.faturas[i]->total_prod;
            total_valor += s->faturas.faturas[i]->valor_total;
        }
        printf("%d %d %.2f\n",
            total_items,
            s->total_faturas,
            arredondamento(total_valor));
        for(i = 0; i < 26; i++){
            if(s->iva_usado[i]){
                printf("%c %d%%\n", 'A'+i, s->iva[i]);
            }
        }
        return 0;
    }
    else{
        char ean[20];
        sscanf(args,"%s",ean);
        if(!validar_ean(ean)){
            printf("invalid ean\n");
            return 0;
        }
        Prod *p = procurar_prod(&s->produtos,ean);
        if(!p){
            printf("%s: no such product\n",ean);
            return 0;
        }
        printf("%d %d %s\n",
            p->stock,
            p->vendido,
            p->descricao);
        return 0;
    }
}

int faturar_prod_cesto(Sistema *s, char *args){
    int nif = 999999999;
    char cliente[101] = "Cliente final";
    int i;
    int total_prod = 0;
    double total = 0;
    if(strlen(args) > 0){
    if(isdigit((unsigned char)args[0])){
        int lidos = sscanf(args,"%d %100[^\n]",&nif,cliente);
        if(lidos >= 1){
            if(!validar_nif(nif)){
                printf("%d: no such nif\n",nif);
                return 0;
            }
        }
        if(lidos == 1){
            strcpy(cliente,"Cliente final");
        }
    }
    else{
        sscanf(args,"%100[^\n]",cliente);
    }
    if (cliente[0] == '"') {
        char *fim = strrchr(cliente, '"');
        if (fim) {
            *fim = '\0';
        }
        memmove(cliente, cliente + 1, strlen(cliente + 1) + 1);
    }
    if(strlen(cliente) > 100) cliente[100] = '\0';
    if(!validar_nome_cliente(cliente)){
        printf("invalid name\n");
        return 0;
    }
}
    if(strcmp(cliente,"error") == 0){
        for(i=0;i<s->cesto.size;i++){
            Prod *p = s->cesto.items[i].produto;
            int q = s->cesto.items[i].quantidade;
            p->stock += q;
            p->vendido -= q;
        }
        s->cesto.size = 0;
        return 0;
    }
    if(s->cesto.size == 0)
        return 0;
    for(i=0;i<s->cesto.size;i++){
        Prod *p = s->cesto.items[i].produto;
        int q = s->cesto.items[i].quantidade;
        total_prod += q;
        total += arredondamento(calcular_iva(s, p->preco,p->iva) * q);
        p->dentro_cesto = 0;
    }
    Fatura *f = malloc(sizeof(Fatura));
    if(!f){
        printf("No memory.\n");
        exit(0);
    }
    f->numero = s->total_faturas + 1;
    s->total_faturas++;
    f->nif = nif;
    f->cliente = copiar_string(cliente);
    f->total_prod = total_prod;
    f->valor_total = arredondamento(total);
    adicionar_fatura(&s->faturas,f);
    s->faturas_ordenadas = 0;
    printf("%d %.2f %d\n",total_prod,f->valor_total,f->numero);
    s->cesto.size = 0;
    return 0;
}

int apagar_registos(Sistema *s, char *args){
    char codigo[20];
    int quantidade;
    int i;
    if(sscanf(args,"%s %d",codigo,&quantidade) == 2){
        if(!validar_ean(codigo)){
            printf("invalid ean\n");
            return 0;
        }
        Prod *p = procurar_prod(&s->produtos,codigo);
        if(!p){
            printf("%s: no such product\n",codigo);
            return 0;
        }
        if(p->dentro_cesto){
            printf("product in use\n");
            return 0;
        }
        if(quantidade <= 0 || quantidade > p->stock){
            printf("invalid quantity\n");
            return 0;
        }
        p->stock -= quantidade;
        printf("%d %s\n",p->stock,p->descricao);
        if (p->stock == 0) {
            int idx_produto = -1;
            for (int j = 0; j < s->produtos.size; j++) {
                if (s->produtos.prods[j] == p) {
                    idx_produto = j;
                    break;
                }
            }
            if (idx_produto != -1) {
                free(p->ean);
                free(p->descricao);
                free(p);
                for (int j = idx_produto; j < s->produtos.size - 1; j++) {
                    s->produtos.prods[j] = s->produtos.prods[j + 1];
                }
                s->produtos.size--;
            }
        }
    }else{
        int numero;
        if(sscanf(args,"%d",&numero) != 1)
            return 0;
        for(i = 0; i<s->faturas.size; i++){
            if(s->faturas.faturas[i]->numero == numero){
                Fatura *f = s->faturas.faturas[i];
                printf("%.2f %d %s\n",
                    f->valor_total,
                    f->nif,
                    f->cliente);
                free(f->cliente);
                free(f);
                for(;i<s->faturas.size-1;i++){
                    s->faturas.faturas[i]=s->faturas.faturas[i+1];
                }
                s->faturas.size--;
                return 0;
            }
        }
        printf("%d: no such invoice\n",numero);
    }
    return 0;
}

int listar_faturas(Sistema *s, char *args){
    int i, encontrou = 0;
    char nome[101];
    if(strlen(args) == 0){
        if (s->faturas.size > 1 && s->faturas_ordenadas == 0) {
            sort_faturas(s->faturas.faturas, s->faturas.size);
            s->faturas_ordenadas = 1; 
        }
        for(i = 0; i < s->faturas.size; i++){
            Fatura *f = s->faturas.faturas[i];
            printf("%d %.2f %s\n",
                f->numero,
                f->valor_total,
                f->cliente);
        }
        return 0;
    }
    sscanf(args,"%100[^\n]",nome);
    if(nome[0] == '"'){
        memmove(nome, nome + 1, strlen(nome));
        char *fim = strrchr(nome,'"');
        if(fim) *fim = '\0';
    }
    if(!validar_nome_cliente(nome)){
        printf("invalid name\n");
        return 0;
    }
    for(i = 0; i < s->faturas.size; i++){
        Fatura *f = s->faturas.faturas[i];
        if(strcmp(f->cliente, nome) == 0){
            printf("%d %.2f %s\n",
                f->numero,
                f->valor_total,
                f->cliente);
            encontrou = 1;
        }
    }
    if(!encontrou)
        printf("%s: no such client\n",nome);
    return 0;
}

void libertar_sistema(Sistema *s){
    int i;
    for(i = 0; i < s->produtos.size; i++){
        free(s->produtos.prods[i]->ean);
        free(s->produtos.prods[i]->descricao);
        free(s->produtos.prods[i]);
    }
    free(s->produtos.prods);
    for(i = 0; i < s->faturas.size; i++){
        free(s->faturas.faturas[i]->cliente);
        free(s->faturas.faturas[i]);
    }
    free(s->faturas.faturas);
    free(s->cesto.items);
}

int main(int argc, char *argv[]){
    char linha[MAX_CARATERES];
    Sistema codigo = {0};
    codigo.total_faturas = 0;
    inicializar_iva(&codigo);
    if(argc > 1)
        ler_iva(&codigo, argv[1]);
    else{
        codigo.iva_usado['A'-'A'] = 1;
        codigo.iva_usado['B'-'A'] = 1;
        codigo.iva_usado['C'-'A'] = 1;
        codigo.iva_usado['D'-'A'] = 1;
    }
    while (fgets(linha, sizeof(linha), stdin)){
        if (linha[0] == '\n' || linha[0] == '\0')
        continue;

    char comando = linha[0];
    char *args = linha + 1;
    while(*args && isspace((unsigned char)*args)){
        args++;
    }
        switch (comando){
            case 'q':
                libertar_sistema(&codigo);
                return 0;
            case 'p':
                introduzir_prod(&codigo, args);
                break;
            case 'l':
                listar_prod(&codigo, args);
                break;
            case 'a':
                adicionar_cesto(&codigo, args);
                break;
            case 'r':
                resumo_faturacao(&codigo, args);
                break;
            case 'f':
                faturar_prod_cesto(&codigo, args);
                break;
            case 'c':
                listar_faturas(&codigo, args);
                break;
            case 'd':
                apagar_registos(&codigo, args);
                break;
            default:
                break;
        }
    }
    return 0;
}