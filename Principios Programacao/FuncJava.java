import java.util.function.*;
import java.util.stream.*;
import java.util.*;
public class FuncJava {
  public static void main(String[] args) {
    int s = 20;
    int menor = -20;
    int inc = 1;
    //int exp = Stream.iterate(menor, n -> n + inc).limit(s).map(x -> - x * x + 2).sorted().toArray()[0];
    System.out.println(Stream.iterate(menor, n -> n + inc).limit(s).map(x -> - x * x + 2).max());
  }
}
