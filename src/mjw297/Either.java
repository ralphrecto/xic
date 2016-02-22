package mjw297;

public abstract class Either<A, B> {
	private Either(){
	}
	
	public abstract boolean isLeft();
	public abstract boolean isRight();

	public static final class Left<A, B> extends Either<A, B> {
		private final A a;
	 	
		Left(final A a) {
			this.a = a;
		}
		
		public boolean isLeft() {
			return true;
		}
		
		public boolean isRight() {
			return false;
		}

		public A getValue() {
			return a;
		}
	}

	public static final class Right<A, B> extends Either<A, B> {
		private final B b;
		
		Right(final B b) {
			this.b = b;
		}
		
		public boolean isLeft() {
			return false;
		}
		
		public boolean isRight() {
			return true;
		}
		
		public B getValue() {
			return b;
		}
	}

	public static <A, B> Either<A, B> left(final A a) {
		return new Left<A, B>(a);	
	}

	public static <A, B> Either<A, B> right(final B b){
		return new Right<A, B>(b);
	}
}


