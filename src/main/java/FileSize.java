import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.ForkJoinTask;
import java.util.concurrent.RecursiveTask;
import java.util.List;
import java.util.ArrayList;
import java.io.File;

public class FileSize {

    private final static ForkJoinPool forkJoinPool = new ForkJoinPool();

    private static class FileSizeFinder extends RecursiveTask<Long> {
        final File file;

        public FileSizeFinder(final File theFile) {
            file = theFile;
        }

        @Override public Long compute() {
            long size = 0;
            if (file.isFile())
                size += file.length();
            else {
                List<ForkJoinTask<Long>> tasks = new ArrayList<>();
                for (File child: file.listFiles()) {
                    if (child.isFile())
                        size += child.length();
                    else {
                        ForkJoinTask<Long> task = new FileSizeFinder(child);
                        tasks.add(task);
                    }
                }

                for (ForkJoinTask<Long> task: invokeAll(tasks)) {
                    size += task.join();
                }
            }

            return size;
        }
    }

    public static void main(final String[] args) {
        final long start = System.nanoTime();
        final long total = forkJoinPool.invoke(new FileSizeFinder(new File("/Users/yao/loyal3/ach-service")));
        final long end = System.nanoTime();
        System.out.println("Total Size: " + total);
        System.out.println("Time taken: " + (end - start)/1.0e9);
    }
}