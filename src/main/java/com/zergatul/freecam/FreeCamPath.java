package com.zergatul.freecam;

import net.minecraft.util.math.Vec3d;

import java.util.ArrayList;
import java.util.List;

public class FreeCamPath {

    private final FreeCam freeCam;
    private final List<Entry> entries = new ArrayList<>();

    // add rotation direction? rotation count

    public FreeCamPath(FreeCam freeCam) {
        this.freeCam = freeCam;
    }

    public boolean add(double time) {
        if (!freeCam.isActive()) {
            return false;
        }

        entries.add(new Entry(
                new Vec3d(freeCam.getX(), freeCam.getY(), freeCam.getZ()),
                freeCam.getXRot(),
                freeCam.getYRot(),
                time));

        return true;
    }

    public void clear() {
        entries.clear();
    }

    public List<Entry> get() {
        return entries;
    }

    public Entry interpolate(double time) {
        for (int i = 1; i < entries.size(); i++) {
            Entry e2 = entries.get(i);
            if (time < e2.time) {
                Entry e1 = entries.get(i - 1);
                double factor = time / e2.time;
                return interpolate(e1, e2, factor);
            } else {
                time -= e2.time;
            }
        }
        return null;
    }

    private Entry interpolate(Entry e1, Entry e2, double factor) {
        Vec3d pos = new Vec3d(
                e1.position.x + (e2.position.x - e1.position.x) * factor,
                e1.position.y + (e2.position.y - e1.position.y) * factor,
                e1.position.z + (e2.position.z - e1.position.z) * factor);
        double xRot = e1.xRot + (e2.xRot - e1.xRot) * factor;
        double yRot = e1.yRot + (e2.yRot - e1.yRot) * factor;
        return new Entry(pos, xRot, yRot, 0);
    }

    public class Entry {

        public final Vec3d position;
        public final double xRot;
        public final double yRot;
        public final double time;

        public Entry(Vec3d position, double xRot, double yRot, double time) {
            this.position = position;
            this.xRot = xRot;
            this.yRot = yRot;
            this.time = time;
        }
    }
}