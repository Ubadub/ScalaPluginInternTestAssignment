import org.scalatra._
import javax.servlet.ServletContext

import server.BooleanTransformationsAPI

class ScalatraBootstrap extends LifeCycle {
  override def init(context: ServletContext) {
    context.mount(new BooleanTransformationsAPI, "/*")
  }
}