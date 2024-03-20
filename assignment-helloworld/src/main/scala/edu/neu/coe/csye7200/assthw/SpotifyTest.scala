package edu.neu.coe.csye7200.assthw

import edu.neu.coe.csye7200.assthw.SpotifyTries.{tryEquals, tryNotEquals}
import spray.json._


import scala.util.{Failure, Success, Try}

object Spotify_Assignment extends App {

  def getSpotifyToken(): String = {

    import MyJsonProtocol._
    val tokenUrl = "https://accounts.spotify.com/api/token"
    val clientId = "6437a219de8f4fe893e23aee452d4a67"
    val clientSecret = "394629b2c759496b87268ef67ddb8a2b"
    val data = Map(
      "grant_type" -> "client_credentials",
      "client_id" -> clientId,
      "client_secret" -> clientSecret
    )
    val headers = Map("Content-Type" -> "application/x-www-form-urlencoded")

    val response = Try(requests.post(tokenUrl, data = data, headers = headers))
    response match {
      case Success(res) =>
        if (res.statusCode == 200) {
          val jsonAst = res.text().parseJson
          val tokenResponse = jsonAst.convertTo[TokenResponse]
          tokenResponse.access_token
        } else {
          println(s"Invalid status: ${res.statusCode}")
          "404"
        }
      case Failure(exception) =>
        println(s"Error making token request: ${exception.getMessage}")
        "404"
    }
  }

  def getSpotify(): Try[Playlist] = {
    import SpotifyJsonFormats._

    val token = "Bearer " + getSpotifyToken();
    val headers = Map("Authorization" -> token)
    for {
      response <- Try(requests.get("https://api.spotify.com/v1/playlists/5Rrf7mqN8uus2AaQQQNdc1", headers = headers))
      _ <- tryEquals(response.statusCode, 200, "invalid status")
      _ <- tryEquals(response.headers("content-type"), List("application/json; charset=utf-8"), "bad content type")
      json <- tryNotEquals(response.text(), "", "empty json")
      spotify <- Try(json.parseJson.convertTo[Playlist])
    } yield spotify
  }

  def getArtistFollowers(artistID: String): Try[Artist] = {
    import SpotifyJsonFormats._

    val token = "Bearer " + getSpotifyToken();
    val headers = Map("Authorization" -> token)
    for {
      response <- Try(requests.get("https://api.spotify.com/v1/artists/" + artistID, headers = headers))
      _ <- tryEquals(response.statusCode, 200, "invalid status")
      _ <- tryEquals(response.headers("content-type"), List("application/json; charset=utf-8"), "bad content type")
      json <- tryNotEquals(response.text(), "", "empty json")
      artistData <- Try(json.parseJson.convertTo[Artist])
    } yield artistData
  }

  val maybeSpotify = getSpotify()

  def getTenLongestTracks(playlistTry: Try[Playlist]): Try[List[(Option[String], Option[Int])]] = {
    for {
      playlist <- playlistTry
      tracksInfo = playlist.tracks.items
        .filter(_.track.duration_ms.isDefined)
        .sortBy(_.track.duration_ms.get)
        .reverse
        .take(10)
        .map(trackItem => (trackItem.track.name, trackItem.track.duration_ms))
    } yield tracksInfo
  }


  val tenLongestTracksInfoTry: Try[List[(Option[String], Option[Int])]] = getTenLongestTracks(maybeSpotify)

  tenLongestTracksInfoTry match {
    case Success(tracksInfo) =>
      tracksInfo.foreach {
        case (Some(name), Some(duration)) => println(s"$name, Duration: $duration ms")
        case (Some(name), None) => println(s"$name, Duration: Unknown")
      }
    case Failure(exception) =>
      println(s"Failed to get track names and durations due to: ${exception.getMessage}")
  }

  def getArtistsFromLongestTracks(playlistTry: Try[Playlist]): Try[List[(String, String)]] = {
    for {
      playlist <- playlistTry
      artistInfo = playlist.tracks.items
        .filter(_.track.duration_ms.isDefined)
        .sortBy(_.track.duration_ms.get)
        .reverse
        .take(10)
        .flatMap(trackItem => trackItem.track.artists.map(artist => (artist.name.getOrElse("Unknown Artist"), artist.id.getOrElse("xyz"))))
    } yield artistInfo
  }


  val artistsTry: Try[List[(String, String)]] = getArtistsFromLongestTracks(maybeSpotify)

  val artistFollowersTry: Try[List[Artist]] = artistsTry.flatMap { artistList =>
    Try(artistList.map { case (_, artistID) =>
      getArtistFollowers(artistID) match {
        case Success(artist) => artist
        case Failure(e) => throw e
      }
    })
  }

  artistFollowersTry match {
    case Success(artists) =>
      artists.foreach { artist =>
        println(s"Artist: ${artist.name}, Followers: ${artist.followers.getOrElse(0)}")
      }
    case Failure(exception) =>
      println(s"Failed to fetch artist followers: ${exception.getMessage}")
  }

}

case class TokenResponse(access_token: String, token_type: String, expires_in: Int)

object MyJsonProtocol extends DefaultJsonProtocol {
  implicit val tokenResponseFormat: RootJsonFormat[TokenResponse] = jsonFormat3(TokenResponse)
}

case class ExternalUrls(spotify: Option[String])

case class Followers(href: Option[String], total: Option[Int])

case class Image(url: Option[String], height: Option[Int], width: Option[Int])

case class Artist(external_urls: ExternalUrls, href: Option[String], id: Option[String], name: Option[String], `type`: Option[String], uri: Option[String],
                  followers: Option[Followers], genres: Option[List[String]], images: Option[List[Image]], popularity: Option[Int])

case class Album(album_type: Option[String], external_urls: ExternalUrls, href: Option[String], id: Option[String], images: List[Image], name: Option[String],
                 release_date: Option[String], release_date_precision: Option[String], `type`: Option[String], uri: Option[String], total_tracks: Option[Int],
                 available_markets: List[String], restrictions: Option[Map[String, String]], artists: List[Artist])

case class Track(album: Album, artists: List[Artist], available_markets: List[String], disc_number: Option[Int], duration_ms: Option[Int], explicit: Option[Boolean],
                 external_ids: Map[String, String], external_urls: ExternalUrls, href: Option[String], id: Option[String], is_playable: Option[Boolean],
                 linked_from: Option[Map[String, String]], name: Option[String], popularity: Option[Int], preview_url: Option[String], track_number: Option[Int],
                 `type`: Option[String], uri: Option[String], is_local: Option[Boolean], restrictions: Option[Map[String, String]])

case class TrackItem(added_at: Option[String], added_by: Artist, is_local: Boolean, track: Track)

case class Tracks(href: Option[String], limit: Option[Int], next: Option[String], offset: Option[Int], previous: Option[String], total: Option[Int], items: List[TrackItem])

case class Playlist(collaborative: Option[Boolean], description: Option[String], external_urls: ExternalUrls, followers: Followers, href: Option[String],
                    id: Option[String], images: List[Image], name: Option[String], owner: Artist, public: Option[Boolean], snapshot_id: Option[String],
                    tracks: Tracks, `type`: Option[String], uri: Option[String])


object SpotifyJsonFormats extends DefaultJsonProtocol {
  implicit val externalUrlsFormat: RootJsonFormat[ExternalUrls] = jsonFormat1(ExternalUrls)
  implicit val followersFormat: RootJsonFormat[Followers] = jsonFormat2(Followers)
  implicit val imageFormat: RootJsonFormat[Image] = jsonFormat3(Image)
  implicit val artistFormat: RootJsonFormat[Artist] = jsonFormat10(Artist)
  implicit val albumFormat: RootJsonFormat[Album] = jsonFormat14(Album)
  implicit val trackFormat: RootJsonFormat[Track] = jsonFormat20(Track)
  implicit val trackItemFormat: RootJsonFormat[TrackItem] = jsonFormat4(TrackItem)
  implicit val tracksFormat: RootJsonFormat[Tracks] = jsonFormat7(Tracks)
  implicit val playlistFormat: RootJsonFormat[Playlist] = jsonFormat14(Playlist)
}

object SpotifyTries {
  def tryMatch[X](f: (X, X) => Boolean)(x: => X, expected: X, message: String): Try[X] =
    if (f(x, expected)) Success(x) else Failure(new Exception(s"$message: $x did not equal $expected"))

  def tryEquals[X](x: => X, expected: X, message: String): Try[X] = tryMatch[X](_ == _)(x, expected, message)

  def tryNotEquals[X](x: => X, expected: X, message: String): Try[X] = tryMatch[X](_ != _)(x, expected, message)
}