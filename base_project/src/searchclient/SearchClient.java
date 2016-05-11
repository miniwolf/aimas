package searchclient;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.*;

import core.*;
import core.Strategy.*;
import core.singleagent.SingleAgent;
import core.singleagent.SingleBox;
import core.singleagent.SingleNode;

public class SearchClient {
    public SingleNode initialState = null;

    public SearchClient(List<String> lines) throws Exception {
        Map<Character, String> colors = new HashMap<>();
        String line, color;

        int colorLines = 0, levelLines = 0;

        // Read lines specifying colors
        int index = 0;
        while ( (line = lines.get(index)).matches("^[a-z]+:\\s*[0-9A-Z](,\\s*[0-9A-Z])*\\s*$") ) {
            line = line.replaceAll("\\s", "");
            String[] colonSplit = line.split(":");
            color = colonSplit[0].trim();

            for ( String id : colonSplit[1].split(",") ) {
                colors.put(id.trim().charAt(0), color);
            }
            colorLines++;
            index++;
        }

        if ( colorLines > 0 ) {
            error("Box colors not supported");
        }

        initialState = new SingleNode(null);
        int id = 0;
        for ( int i = index; i < lines.size(); i++ ) {
            line = lines.get(i);
            for ( int j = 0; j < line.length(); j++ ) {
                char chr = line.charAt(j);
                Position pos = new Position(j, levelLines);
                if ( '+' == chr ) { // Walls
                    Node.walls.add(pos);
                } else if ( '0' <= chr && chr <= '9' ) { // Agents
                    initialState.setAgent(new SingleAgent(pos, 0)); // TODO: Does not support MA
                } else if ( 'A' <= chr && chr <= 'Z' ) { // Boxes
                    initialState.getBoxes().add(new SingleBox(pos, chr, id));
                    id++;
                } else if ( 'a' <= chr && chr <= 'z' ) { // Goal cells
                    Node.goals.put(pos, chr);
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
