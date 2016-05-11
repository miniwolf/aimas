package searchclient;

import core.Agent;
import core.Box;
import core.Position;
import core.multiagent.MultiAgent;
import core.multiagent.MultiBox;
import core.multiagent.MultiNode;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class MultiSearchClient {
    public MultiNode initialState = null;

    public List<Agent> agents = new ArrayList<>();

    public MultiSearchClient(List<String> lines) throws Exception {
        Map<Character, String> colors = new HashMap<>();
        String line, color;

        int levelLines = 0;

        // Read lines specifying colors
        int index = 0;
        while ( (line = lines.get(index)).matches("^[a-z]+:\\s*[0-9A-Z](,\\s*[0-9A-Z])*\\s*$") ) {
            line = line.replaceAll("\\s", "");
            String[] colonSplit = line.split(":");
            color = colonSplit[0].trim();

            for ( String id : colonSplit[1].split(",") ) {
                colors.put(id.trim().charAt(0), color);
            }
            index++;
        }

        initialState = new MultiNode(null);
        int id = 0;
        for ( int i = index; i < lines.size(); i++ ) {
            line = lines.get(i);
            for ( int j = 0; j < line.length(); j++ ) {
                char chr = line.charAt(j);
                Position pos = new Position(j, levelLines);
                if ( '+' == chr ) { // Walls
                    MultiNode.walls.add(pos);
                } else if ( '0' <= chr && chr <= '9' ) { // Agents
                    MultiAgent agent = new MultiAgent(pos, Character.getNumericValue(chr), colors.get(chr));
                    initialState.putAgent(agent);
                } else if ( 'A' <= chr && chr <= 'Z' ) { // Boxes
                    initialState.boxes.add(new MultiBox(pos, chr, id, colors.get(chr)));
                    id++;
                } else if ( 'a' <= chr && chr <= 'z' ) { // Goal cells
                    MultiNode.goals.put(pos, chr);
                }
            }
            levelLines++;
        }
    }

    // Auxiliary static classes
    public static void error(String msg) throws Exception {
        throw new Exception("GSCError: " + msg);
    }

    public static class Memory {
        public static final float mb = 1024 * 1024;
        public static final float limitRatio = .9f;
        public static final int timeLimit = 180;
        public static Runtime runtime = Runtime.getRuntime();

        public static float used() {
            return (runtime.totalMemory() - runtime.freeMemory()) / mb;
        }

        public static float free() {
            return runtime.freeMemory() / mb;
        }

        public static float total() {
            return runtime.totalMemory() / mb;
        }

        public static float max() {
            return runtime.maxMemory() / mb;
        }

        public static boolean shouldEnd() {
            return (used() / max() > limitRatio);
        }

        public static String stringRep() {
            return String.format("[Used: %.2f MB, Free: %.2f MB, Alloc: %.2f MB, MaxAlloc: %.2f MB]", used(), free(), total(), max());
        }
    }
}