package ex2

import org.junit.*
import org.junit.Assert.*

class ConferenceReviewingTest:

  val cr: ConferenceReviewing = ConferenceReviewing()

  cr.loadReview(1, 8, 8, 6, 8) // 4.8 è il voto finale pesato (usato da averageWeightedFinalScoreMap)
  cr.loadReview(1, 9, 9, 6, 9) // 5.4
  cr.loadReview(2, 9, 9, 10, 9) // 9.0
  cr.loadReview(2, 4, 6, 10, 6) // 6.0
  cr.loadReview(3, 3, 3, 3, 3) // 0.9
  cr.loadReview(3, 4, 4, 4, 4) // 1.6
  cr.loadReview(4, 6, 6, 6, 6) // 3.6
  cr.loadReview(4, 7, 7, 8, 7) // 5.6

  import Question.*
  val map: Map[Question, Int] = Map(RELEVANCE -> 8, SIGNIFICANCE -> 8, CONFIDENCE -> 7, FINAL -> 8)
  cr.loadReview(4, map)
  cr.loadReview(5, 6, 6, 6, 10) // 6.0
  cr.loadReview(5, 7, 7, 7, 10) // 7.0

  @Test def testOrderedScores(): Unit =
    assertEquals(List(4, 9), cr.orderedScores(2, RELEVANCE))
    assertEquals(List(6, 7, 8), cr.orderedScores(4, CONFIDENCE))
    assertEquals(List(10, 10), cr.orderedScores(5, FINAL))

  @Test def testAverageFinalScore(): Unit =
    assertEquals(8.5, cr.averageFinalScore(1), 0.01)
    assertEquals(7.5, cr.averageFinalScore(2), 0.01)
    assertEquals(3.5, cr.averageFinalScore(3), 0.01)
    assertEquals(7.0, cr.averageFinalScore(4), 0.01)
    assertEquals(10.0, cr.averageFinalScore(5), 0.01)

  @Test def testAcceptedArticles(): Unit =
    assertEquals(Set(1, 2, 4), cr.acceptedArticles())

  /*@org.junit.Test
  public void testSortedAcceptedArticles() {
    // articoli accettati, e loro voto finale medio
    assertEquals(cr.sortedAcceptedArticles(),Arrays.asList(new Pair<>(4,7.0),new Pair<>(2,7.5),new Pair<>(1,8.5)))
  }

  @org.junit.Test
  public void optionalTestAverageWeightedFinalScore() {
    // l'articolo 1 ha media pesata finale pari a (4.8+5.4)/2 = 5,1, con scarto massimo 0.01
    assertEquals(cr.averageWeightedFinalScoreMap().get(1),(4.8+5.4)/2,0.01)
    // e simile per gli altri
    assertEquals(cr.averageWeightedFinalScoreMap().get(2),(9.0+6.0)/2,0.01)
    assertEquals(cr.averageWeightedFinalScoreMap().get(3),(0.9+1.6)/2,0.01)
    assertEquals(cr.averageWeightedFinalScoreMap().get(4),(3.6+5.6+5.6)/3,0.01)
    assertEquals(cr.averageWeightedFinalScoreMap().get(5),(6.0+7.0)/2,0.01)
    assertEquals(cr.averageWeightedFinalScoreMap().size(),5)*/

