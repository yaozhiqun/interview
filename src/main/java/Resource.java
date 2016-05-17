/**
 * Created by Zachary Yao on 5/5/16.
 */
public class Resource {

  private volatile Resource resource;

  public Resource getResource() {
    Resource result = resource;

    if (result == null) {
      synchronized (this) {
        result = resource;
        if (result == null) {
          resource = result = new Resource();
        }
      }
    }
    return resource;
  }
}
