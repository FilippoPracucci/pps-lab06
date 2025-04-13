package ex2

case class Pair[A, B](x: A, y: B)

enum Question:
  case RELEVANCE, SIGNIFICANCE, CONFIDENCE, FINAL

trait ConferenceReviewing:
  def loadReview(article: Int, scores: Map[Question, Int]): Unit
  def loadReview(article: Int, relevance: Int, significance: Int, confidence: Int, fin: Int): Unit
  def orderedScores(article: Int, question: Question): List[Int]
  def averageFinalScore(article: Int): Double
  def acceptedArticles(): Set[Int]
  def sortedAcceptedArticles(): List[Pair[Int, Double]]
  def averageWeightedFinalScoreMap(): Map[Int, Double]

object ConferenceReviewing:
  def apply(): ConferenceReviewing = ConferenceReviewingImpl()

  import Question.*
  extension (list: List[Int])
    private def avg: Double =
      list.map(_.toDouble).sum./(list.size)
  extension (list: List[Pair[Int, Map[Question, Int]]])
    private def avgWeighted: Double =
      list.map(p => p.y(CONFIDENCE).toDouble * p.y(FINAL).toDouble / 10).sum./(list.size)

  private case class ConferenceReviewingImpl() extends ConferenceReviewing:
    import Question.*
    private var _reviews: List[Pair[Int, Map[Question, Int]]] = List.empty

    override def loadReview(article: Int, scores: Map[Question, Int]): Unit =
      _reviews = Pair(article, scores) :: _reviews

    override def loadReview(article: Int, relevance: Int, significance: Int, confidence: Int, fin: Int): Unit =
      _reviews = Pair(article,
        Map(RELEVANCE -> relevance, SIGNIFICANCE -> significance, CONFIDENCE -> confidence, FINAL -> fin)) :: _reviews

    override def orderedScores(article: Int, question: Question): List[Int] =
      _reviews.collect { case Pair(a, m) if a == article => m(question) }.sorted

    override def averageFinalScore(article: Int): Double =
      _reviews.collect { case Pair(a, m) if a == article => m(FINAL) }.avg

    override def acceptedArticles(): Set[Int] =
      _reviews.collect { case Pair(a, _) if averageFinalScore(a) > 5 && orderedScores(a, RELEVANCE).exists(_ >= 8) => a }.toSet

    override def sortedAcceptedArticles(): List[Pair[Int, Double]] =
      _reviews.collect{ case Pair(a, _) if acceptedArticles().contains(a) => Pair(a, averageFinalScore(a)) }.distinct

    override def averageWeightedFinalScoreMap(): Map[Int, Double] =
      _reviews.groupBy(_._1).map((a, l) => (a, l.avgWeighted)).toMap()
