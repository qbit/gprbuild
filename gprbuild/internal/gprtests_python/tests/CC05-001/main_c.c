extern void pack__foo (void);
extern void adainit (void);
extern void adafinal (void);

int main (void) {
  adainit();
  pack__foo();
  pack2__bar();
  adafinal();
}
