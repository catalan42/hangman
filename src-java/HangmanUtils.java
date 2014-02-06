public class HangmanUtils {
  public static boolean isGameWon( HangmanGame game ) {
    return ( game.gameStatus() == HangmanGame.Status.GAME_WON ); }

  public static boolean isGameLost( HangmanGame game ) {
    return ( game.gameStatus() == HangmanGame.Status.GAME_LOST ); }

  public static boolean isKeepGuessing( HangmanGame game ) {
    return ( game.gameStatus() == HangmanGame.Status.KEEP_GUESSING ); }
}
