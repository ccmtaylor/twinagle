package com.soundcloud.twinagle

import com.twitter.finagle._
import com.twitter.finagle.http.{Request, Response}
import com.twitter.finagle.tracing.{ClientTracingFilter, ServerTracingFilter, Trace}
import com.twitter.util.Future

case class TracingFilter(endpointMetadata: EndpointMetadata, isClient: Boolean) extends SimpleFilter[Request, Response] {
  override def apply(request: Request, service: Service[Request, Response]): Future[Response] = {
    val trace = Trace()
    trace.recordBinary(TracingFilter.SpanKind, if (isClient) "client" else "server")
    trace.recordBinary(TracingFilter.Prefix, endpointMetadata.prefix)
    trace.recordBinary(TracingFilter.Service, endpointMetadata.service)
    trace.recordBinary(TracingFilter.Rpc, endpointMetadata.rpc)

    service(request).onSuccess { response =>
      trace.recordBinary(TracingFilter.StatusCode, response.statusCode)
    }.onFailure { ex =>
      trace.recordBinary(TracingFilter.StatusCode, 500)
      trace.recordBinary(TracingFilter.Error, true)
    }
  }
}

object TracingFilter {

  val Service = "twirp.service"
  val Prefix = "twirp.prefix"
  val Rpc = "twirp.rpc"
  val StatusCode = "twirp.error_code" // TODO: can we make this be based on twirp error codes instead of http response codes?

  val SpanKind = "span.kind" // "server" or "client"
  val Error = "error" // bool

  def clientModule(endpointMetadata: EndpointMetadata): Stackable[ServiceFactory[Request, Response]] =
    new Stack.Module1[param.Tracer, ServiceFactory[Request, Response]] {
      val role: Stack.Role = ClientTracingFilter.role
      val description = "Report finagle information and client send/recv events"

      def make(_tracer: param.Tracer,
               next: ServiceFactory[Request, Response]
              ): ServiceFactory[Request, Response] = {
        val param.Tracer(tracer) = _tracer
        if (tracer.isNull) {
          next
        } else {
          ClientTracingFilter.TracingFilter[Request, Response](endpointMetadata.path) andThen
            TracingFilter(endpointMetadata, isClient = true) andThen
            next
        }
      }
    }

  def serverModule(endpointMetadata: EndpointMetadata): Stackable[ServiceFactory[Request, Response]] =
    new Stack.Module1[param.Tracer, ServiceFactory[Request, Response]] {
      val role: Stack.Role = ClientTracingFilter.role
      val description = "Report finagle information and client send/recv events"

      def make(_tracer: param.Tracer,
               next: ServiceFactory[Request, Response]
              ): ServiceFactory[Request, Response] = {
        val param.Tracer(tracer) = _tracer
        if (tracer.isNull) {
          next
        } else {
          ServerTracingFilter.TracingFilter[Request, Response](endpointMetadata.path) andThen
            TracingFilter(endpointMetadata, isClient = false) andThen
            next
        }
      }
    }

}
