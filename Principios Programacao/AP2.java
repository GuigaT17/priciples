public class AP2 implements Funcao<Integer, Integer> {
  public <A> A aplicar2Vezes (Funcao<A,A> f, A x) {
    return f.aplicar(f.aplicar(x));
  }
}
