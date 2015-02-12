#include <stdio.h>
#include <unistd.h> 
int main()
{
    printf("Hola! Trataré de adivinar un número.\n");

 printf("Piensa un número entre 1 y 10.\n");
    sleep(5); 
    printf("Ahora multiplícalo por 9.\n");
    sleep(5); 
    printf("Si el número tiene 2 dígitos, súmalos entre si: Ej. 36 -> 3+6=9. Si tu número tiene un solo dígito, súmale 0.\n");
    sleep(5); 
    printf("Al número resultante súmale 4.\n");
    sleep(10); 
    printf("Muy bien. El resultado es 13 :) \n");


}
