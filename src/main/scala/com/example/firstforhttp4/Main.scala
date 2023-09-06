package com.example.firstforhttp4

import cats.effect.{IO, IOApp}

object Main extends IOApp.Simple {
  val run = Firstforhttp4Server.run[IO]
}
