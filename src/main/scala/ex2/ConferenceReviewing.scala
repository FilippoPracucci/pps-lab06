package ex2

case class Pair[A, B](x: A, y: B)

enum Question:
  case RELEVANCE, SIGNIFICANCE, CONFIDENCE, FINAL

trait ConferenceReviewing:
  def reviews: List[(Int, Map[Question, Int])]
  def reviews_=(review: (Int, Map[Question, Int])): Unit
  def loadReview(article: Int, scores: Map[Question, Int]): Unit
  def loadReview(article: Int, relevance: Int, significance: Int, confidence: Int, fin: Int): Unit
  def orderedScores(article: Int, question: Question): List[Int]
  def averageFinalScore(article: Int): Double
  def acceptedArticles(): Set[Int]
  def sortedAcceptedArticles(): List[Pair[Int, Double]]
  def averageWeightedFinalScoreMap(): Map[Int, Double]

object ConferenceReviewing:
  def apply(): ConferenceReviewing = ConferenceReviewingImpl()
  private case class ConferenceReviewingImpl() extends ConferenceReviewing:
    import Question.*
    private var _reviews: List[(Int, Map[Question, Int])] = List.empty

    override def reviews: List[(Int, Map[Question, Int])] = _reviews
    override def reviews_=(review: (Int, Map[Question, Int])): Unit = _reviews = review :: _reviews
    
    override def loadReview(article: Int, scores: Map[Question, Int]): Unit =
      reviews = (article, scores)

    override def loadReview(article: Int, relevance: Int, significance: Int, confidence: Int, fin: Int): Unit =
      reviews = (article,
        Map(RELEVANCE -> relevance, SIGNIFICANCE -> significance, CONFIDENCE -> confidence, FINAL -> fin))

    override def orderedScores(article: Int, question: Question): List[Int] =
      _reviews.filter(_._1 == article).map((_, m) => m(question)).sorted // can be improved with foldRight

    extension (list: List[Int])
      private def avg: Double = list.map(_.toDouble).sum./(list.size)

    override def averageFinalScore(article: Int): Double =
      _reviews.filter(_._1 == article).map(_._2(FINAL)).avg

    override def acceptedArticles(): Set[Int] =
      _reviews.filter((a, m) => averageFinalScore(a) > 5 && orderedScores(a, RELEVANCE).exists(_ >= 8)).map(_._1).toSet

    override def sortedAcceptedArticles(): List[Pair[Int, Double]] = ???

    override def averageWeightedFinalScoreMap(): Map[Int, Double] = ???