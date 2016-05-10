package searchclient;

import java.util.LinkedList;
import java.util.List;

/**
 * @author miniwolf
 */
public class SearchResult {
    private List<Node> solution;
    private boolean reachedThreshold;

    public SearchResult(List<Node> solution, boolean reachedThreshold) {
        this.solution = solution;
        this.reachedThreshold = reachedThreshold;
    }

    public List<Node> getSolution() {
        return solution;
    }

    public boolean isReachedThreshold() {
        return reachedThreshold;
    }
}
