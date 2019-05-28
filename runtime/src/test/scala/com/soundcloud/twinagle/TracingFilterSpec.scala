package com.soundcloud.twinagle

import com.twitter.finagle.Service
import com.twitter.finagle.http.{Method, Request, Response, Status}
import com.twitter.finagle.tracing.{Annotation, BufferingTracer, Trace}
import com.twitter.util.{Await, Future}
import org.specs2.mutable.Specification
import org.specs2.specification.Scope

class TracingFilterSpec extends Specification {
  trait Context extends Scope {
    val tracer = new BufferingTracer
    val response = Response()
    val request = Request(Method.Post, "/twirp/svc/rpc")

    def binaryAnnotations = tracer.map(_.annotation).collect {
      case Annotation.BinaryAnnotation(k, v) => (k, v)
    }.toMap

  }

  "adds annotations" >> {
    "successful request" in new Context {
      Trace.letTracer(tracer) {
        val svc = new TracingFilter(EndpointMetadata("/twirp", "svc", "rpc"), isClient = true) andThen
          Service.const(Future.value(response))
        Await.result(svc(request))
      }

      binaryAnnotations.get(TracingFilter.SpanKind) ==== Some("client")
      binaryAnnotations.get(TracingFilter.Error) ==== None

      binaryAnnotations.get(TracingFilter.Prefix) ==== Some("/twirp")
      binaryAnnotations.get(TracingFilter.Service) ==== Some("svc")
      binaryAnnotations.get(TracingFilter.Rpc) ==== Some("rpc")
      binaryAnnotations.get(TracingFilter.StatusCode) ==== Some(200) // TODO: note: status code
    }

    "not found response" in new Context {
      Trace.letTracer(tracer) {
        val svc = new TracingFilter(EndpointMetadata("/twirp", "svc", "rpc"), isClient = true) andThen
          Service.const(Future.value(Response(Status(404))))
        Await.result(svc(request))
      }

      binaryAnnotations.get(TracingFilter.Error) ==== None // TODO: does >= 400 mean error?
      binaryAnnotations.get(TracingFilter.StatusCode) ==== Some(404) // TODO: note: status code
    }

    "errors" in new Context {
      val exception = new RuntimeException("oops")
      Trace.letTracer(tracer) {
        val svc = new TracingFilter(EndpointMetadata("/twirp", "svc", "rpc"), isClient = false) andThen
          Service.const(Future.exception(exception))
        Await.result(svc(request).liftToTry)
      }

      binaryAnnotations.get(TracingFilter.SpanKind) ==== Some("server")
      binaryAnnotations.get(TracingFilter.Error) ==== Some(true)

      binaryAnnotations.get(TracingFilter.Prefix) ==== Some("/twirp")
      binaryAnnotations.get(TracingFilter.Service) ==== Some("svc")
      binaryAnnotations.get(TracingFilter.Rpc) ==== Some("rpc")
      binaryAnnotations.get(TracingFilter.StatusCode) ==== Some(500) // TODO: note: status code
    }
  }
}
