package generator;

import com.trolltech.qt.QNativePointer;
import com.trolltech.qt.QtBlockedSlot;
import com.trolltech.qt.core.QPoint;
import com.trolltech.qt.core.QPointF;
import com.trolltech.qt.gui.*;

class QTransform___ extends QTransform {

    public final QTransform multiply(double d) {
        operator_multiply_assign(d);
        return this;
    }

    public final QTransform add(double d) {
        operator_add_assign(d);
        return this;
    }

    public final QTransform divide(double d) {
        operator_divide_assign(d);
        return this;
    }

    public final QTransform subtract(double d) {
        operator_subtract_assign(d);
        return this;
    }

    /**
     * Returns an inverted copy of this transformation.
     * 
     * @return The inverse of the transformation.
     * @throws IllegalArgumentException
     *             If this transformation is not invertible.
     */
    public final QTransform inverted() {
        QNativePointer ok = new QNativePointer(QNativePointer.Type.Boolean);
        QTransform returned = inverted(ok);
        if (!ok.booleanValue())
            throw new IllegalArgumentException("Transformation is not invertible");
        return returned;
    }

    /**
     * Creates a transformation mapping one arbitrary quad into another.
     * 
     * @return The transformation.
     * @throws IllegalArgumentException
     *             If this transformation is not possible.
     */
    public static final QTransform quadToQuad(QPolygonF from, QPolygonF to) {
        QTransform res = new QTransform();
        QNativePointer resPointer = res.nativePointer();
        if (quadToQuadPrivate(from, to, resPointer)) {
            return res;
        } else
            throw new IllegalArgumentException("Transformation is not possible");
    }

    /**
     * Creates a transformation that maps a quad to a unit square.
     *
     * @return The transformation.
     * @throws IllegalArgumentException If this transformation is not possible.
     */
    public static final QTransform quadToSquare(QPolygonF quad) {
        QTransform res = new QTransform();
        QNativePointer resPointer = res.nativePointer();
        if (quadToSquarePrivate(quad, resPointer)) {
            return res;
        } else
            throw new IllegalArgumentException("Transformation is not possible");
    }

    /**
     * Creates a transformation that maps a unit square to a the given quad.
     * 
     * @return The transformation.
     * @throws IllegalArgumentException
     *             If this transformation is not possible.
     */
    public static final QTransform squareToQuad(QPolygonF quad) {
        QTransform res = new QTransform();
        QNativePointer resPointer = res.nativePointer();
        if (squareToQuadPrivate(quad, resPointer)) {
            return res;
        } else
            throw new IllegalArgumentException("Transformation is not possible");
    }
}// class

class QBitmap___ extends QBitmap {

    public QBitmap(String fileName, String format) {
        this(fileName, format == null ? null : com.trolltech.qt.QNativePointer.createCharPointer(format));
    }

    public QBitmap(String fileName) {
        this(fileName, (String) null);
    }

    public static QBitmap fromData(com.trolltech.qt.core.QSize size, byte bits[], QImage.Format monoFormat) {
        return fromData(size, QtJambiInternal.byteArrayToNativePointer(bits), monoFormat);
    }

    public static QBitmap fromData(com.trolltech.qt.core.QSize size, byte bits[]) {
        return fromData(size, bits, QImage.Format.Format_MonoLSB);
    }
}// class

class QPolygon___ extends QPolygon {

    private native void add_private(long nid, int x, int y);

    @QtBlockedSlot
    public final QPolygon add(int x, int y) {
        add_private(nativeId(), x, y);
        return this;
    }

    @QtBlockedSlot
    public final QPolygon add(QPoint pt) {
        add_private(nativeId(), pt.x(), pt.y());
        return this;
    }

    @QtBlockedSlot
    public final QPolygon add(QPolygon p) {
        int size = p.size();
        long nid = nativeId();
        for (int i = 0; i < size; ++i) {
            QPoint pt = p.at(i);
            add_private(nid, pt.x(), pt.y());
        }
        return this;
    }
}// class

class QPolygonF___ extends QPolygonF {
    private native void add_private(long nid, double x, double y);

    @QtBlockedSlot
    public final QPolygonF add(double x, double y) {
        add_private(nativeId(), x, y);
        return this;
    }

    @QtBlockedSlot
    public final QPolygonF add(QPointF pt) {
        add_private(nativeId(), pt.x(), pt.y());
        return this;
    }

    @QtBlockedSlot
    public final QPolygonF add(QPolygonF p) {
        int size = p.size();
        long nid = nativeId();
        for (int i = 0; i < size; ++i) {
            QPointF pt = p.at(i);
            add_private(nid, pt.x(), pt.y());
        }
        return this;
    }
}// class

class QTreeWidgetItemIterator___ extends QTreeWidgetItemIterator {
    @QtBlockedSlot
    public final void next(int i) {
        operator_add_assign(i);
    }

    @QtBlockedSlot
    public final void previous(int i) {
        operator_subtract_assign(i);
    }

    @QtBlockedSlot
    public final void next() {
        operator_increment();
    }

    @QtBlockedSlot
    public final void previous() {
        operator_decrement();
    }

    @QtBlockedSlot
    public final QTreeWidgetItem current() {
        return operator_multiply();
    }
}// class

class QTextCursor___ extends QTextCursor {
    public final QTableArea selectedTableCells() {
        QNativePointer firstRow = new QNativePointer(QNativePointer.Type.Int);
        QNativePointer numRows = new QNativePointer(QNativePointer.Type.Int);
        QNativePointer firstColumn = new QNativePointer(QNativePointer.Type.Int);
        QNativePointer numColumns = new QNativePointer(QNativePointer.Type.Int);

        selectedTableCells(firstRow, numRows, firstColumn, numColumns);

        return new QTableArea(firstRow.intValue(), firstColumn.intValue(), numRows.intValue(), numColumns.intValue());
    }
}// class

class QMatrix___ extends QMatrix {
    /**
     * Returns an inverted copy of this matrix.
     * 
     * @return The inverse of the matrix.
     * @throws IllegalArgumentException
     *             If this matrix is not invertible.
     */
    public final QMatrix inverted() {
        QNativePointer ok = new QNativePointer(QNativePointer.Type.Boolean);
        QMatrix returned = inverted(ok);
        if (!ok.booleanValue())
            throw new IllegalArgumentException("Matrix is not invertible");
        return returned;
    }

    @QtBlockedSlot
    public final QMatrix multiply(QMatrix other) {
        operator_multiply_assign(other);
        return this;
    }

    @QtBlockedSlot
    public final QMatrix multiplied(QMatrix other) {
        return operator_multiply(other);
    }
}// class

class QImage___ extends QImage {
    public QImage(String xpm[]) {
        this(com.trolltech.qt.QNativePointer.createCharPointerPointer(xpm));
    }

    public final byte[] copyOfBytes() {
        QNativePointer bits = bits();
        byte bytes[] = new byte[numBytes()];
        for (int i = 0; i < bytes.length; ++i)
            bytes[i] = bits.byteAt(i);
        return bytes;
    }

    public QImage(byte data[], int width, int height, Format format) {
        this(com.trolltech.qt.QtJambiInternal.byteArrayToNativePointer(data), width, height, format);
    }

    public QImage(String fileName, String format) {
        this(fileName, format == null ? null : QNativePointer.createCharPointer(format));
    }

    public QImage(String fileName) {
        this(fileName, (String) null);
    }
}// class

class QPen___ extends QPen {
    public QPen(QColor color, double width, com.trolltech.qt.core.Qt.PenStyle s, com.trolltech.qt.core.Qt.PenCapStyle c, com.trolltech.qt.core.Qt.PenJoinStyle j) {
        this(new QBrush(color), width, s, c, j);
    }

    public QPen(QColor color, double width, com.trolltech.qt.core.Qt.PenStyle s, com.trolltech.qt.core.Qt.PenCapStyle c) {
        this(new QBrush(color), width, s, c);
    }

    public QPen(QColor color, double width, com.trolltech.qt.core.Qt.PenStyle s) {
        this(new QBrush(color), width, s);
    }

    public QPen(QColor color, double width) {
        this(new QBrush(color), width);
    }

    public static final QPen NoPen = new QPen(com.trolltech.qt.core.Qt.PenStyle.NoPen);
}// Class

class QColor___ extends QColor {
    public static final QColor white = new QColor(com.trolltech.qt.core.Qt.GlobalColor.white);
    public static final QColor black = new QColor(com.trolltech.qt.core.Qt.GlobalColor.black);
    public static final QColor red = new QColor(com.trolltech.qt.core.Qt.GlobalColor.red);
    public static final QColor darkRed = new QColor(com.trolltech.qt.core.Qt.GlobalColor.darkRed);
    public static final QColor green = new QColor(com.trolltech.qt.core.Qt.GlobalColor.green);
    public static final QColor darkGreen = new QColor(com.trolltech.qt.core.Qt.GlobalColor.darkGreen);
    public static final QColor blue = new QColor(com.trolltech.qt.core.Qt.GlobalColor.blue);
    public static final QColor darkBlue = new QColor(com.trolltech.qt.core.Qt.GlobalColor.darkBlue);
    public static final QColor cyan = new QColor(com.trolltech.qt.core.Qt.GlobalColor.cyan);
    public static final QColor darkCyan = new QColor(com.trolltech.qt.core.Qt.GlobalColor.darkCyan);
    public static final QColor magenta = new QColor(com.trolltech.qt.core.Qt.GlobalColor.magenta);
    public static final QColor darkMagenta = new QColor(com.trolltech.qt.core.Qt.GlobalColor.darkMagenta);
    public static final QColor yellow = new QColor(com.trolltech.qt.core.Qt.GlobalColor.yellow);
    public static final QColor darkYellow = new QColor(com.trolltech.qt.core.Qt.GlobalColor.darkYellow);
    public static final QColor gray = new QColor(com.trolltech.qt.core.Qt.GlobalColor.gray);
    public static final QColor darkGray = new QColor(com.trolltech.qt.core.Qt.GlobalColor.darkGray);
    public static final QColor lightGray = new QColor(com.trolltech.qt.core.Qt.GlobalColor.lightGray);
    public static final QColor transparent = new QColor(com.trolltech.qt.core.Qt.GlobalColor.transparent);
    public static final QColor color0 = new QColor(com.trolltech.qt.core.Qt.GlobalColor.color0);
    public static final QColor color1 = new QColor(com.trolltech.qt.core.Qt.GlobalColor.color1);

}// class

class QTextLine___ extends QTextLine {

    public final void draw(QPainter painter, com.trolltech.qt.core.QPointF position) {
        draw(painter, position, null);
    }

}// class

class QLineF___ extends QLineF {

    public final QLineF.IntersectType intersect(QLineF line, com.trolltech.qt.core.QPointF intersectionPoint) {
        return intersect(line, intersectionPoint != null ? intersectionPoint.nativePointer() : null);
    }

}// class

class QKeySequence___ extends QKeySequence {

    @QtBlockedSlot
    public final int toInt() {
        return operator_cast_int();
    }

    @QtBlockedSlot
    public final int at(int i) {
        return operator_subscript(i);
    }

}// class

class QPicture___ extends QPicture {

    public final boolean load(QIODevice dev) {
        return load(dev, (QNativePointer) null);
    }

    public final boolean load(String fileName) {
        return load(fileName, (com.trolltech.qt.QNativePointer) null);
    }

    public final boolean save(QIODevice dev) {
        return save(dev, (com.trolltech.qt.QNativePointer) null);
    }

    public final boolean save(String fileName) {
        return save(fileName, (com.trolltech.qt.QNativePointer) null);
    }

    public final byte[] data() {
        QNativePointer npData = data_private();
        if (npData == null)
            return null;
        byte returned[] = new byte[size()];
        for (int i = 0; i < returned.length; ++i)
            returned[i] = npData.byteAt(i);
        return returned;
    }

}// class

class QRegion___ extends QRegion {

    public void setRects(com.trolltech.qt.core.QRect[] rects) {
        setRects(com.trolltech.qt.core.QRect.nativePointerArray(rects), rects.length);
    }

}// class

class QPolygon___ extends QPolygon {

}// class

class QPolygonF___ extends QPolygonF {

}// class

class QTextFrame_iterator___ extends QTextFrame_iterator {

    @QtBlockedSlot
    public final void next() {
        operator_increment();
    }

    @QtBlockedSlot
    public final void previous() {
        operator_decrement();
    }

}// class

class QTextBlock_iterator___ extends QTextBlock_iterator {

    @QtBlockedSlot
    public final void next() {
        operator_increment();
    }

    @QtBlockedSlot
    public final void previous() {
        operator_decrement();
    }

}// class

class QPixmap___ extends QPixmap {

    public QPixmap(String xpm[]) {
        this(com.trolltech.qt.QNativePointer.createCharPointerPointer(xpm));
    }

}// class

class QItemSelection___ extends QItemSelection {

    public static void split(QItemSelectionRange range, QItemSelectionRange other, QItemSelection result) {
        com.trolltech.qt.QNativePointer np = result.nativePointer();
        split(range, other, np);
    }

}// class

class QPainterPath_Element___ extends QPainterPath_Element {

    @QtBlockedSlot
    public final com.trolltech.qt.core.QPointF toPoint() {
        return operator_cast_QPointF();
    }

}// class

class QBrush___ extends QBrush {

    public static final QBrush NoBrush = new QBrush(com.trolltech.qt.core.Qt.BrushStyle.NoBrush);

}// class

class QAbstractItemView___ extends QAbstractItemView {

    private java.util.Hashtable<Integer, QAbstractItemDelegate> __rcDelegatesForColumns = new java.util.Hashtable<Integer, QAbstractItemDelegate>();
    private java.util.Hashtable<Integer, QAbstractItemDelegate> __rcDelegatesForRows = new java.util.Hashtable<Integer, QAbstractItemDelegate>();

}// class

class QAccessibleTableInterface___ extends QAccessibleTableInterface {

    public static class CellAtIndex extends QTableArea {
        public CellAtIndex(int row, int column, int rowSpan, int columnSpan, boolean isSelected) {
            super(row, column, rowSpan, columnSpan);
            this.isSelected = isSelected;
        }

        public boolean isSelected;
    }

}// class

class QAccessibleInterface___ extends QAccessibleInterface {

    public static class Target {
        public Target(int childIndex, QAccessibleInterface target) {
            this.childIndex = childIndex;
            this.target = target;
        }

        public QAccessibleInterface target;
        public int childIndex;
    }

}// class

class QDesktopServices___ extends QDesktopServices {

    public static void setUrlHandler(String scheme, com.trolltech.qt.core.QObject receiver, String method) {
        setUrlHandler(scheme, receiver, QNativePointer.createCharPointer(method));
    }

}// class

class QWizardPage___ extends QWizardPage {

    protected final void registerField(String name, QWidget widget) {
        registerField(name, widget, (com.trolltech.qt.QNativePointer) null, (com.trolltech.qt.QNativePointer) null);
    }

    protected final void registerField(String name, QWidget widget, String property) {
        registerField(name, widget, QNativePointer.createCharPointer(property), null);
    }

    /**
     * Creates a field called <code>name</code> associated with the given
     * property of the given widget. From then on, that property becomes
     * accessible using <code>field()</code> and <code>setField()</code>.
     * Fields are global to the entire wizard and make it easy for any single
     * page to access information stored by another page, without having to put
     * all the logic in <code>QWizard</code> or having the pages know
     * explicitly about each other. If name ends with an asterisk (*), the field
     * is a mandatory field. When a page has mandatory fields, the Next and/or
     * Finish buttons are enabled only when all mandatory fields are filled.
     * This requires a <code>changedSignal</code> to be specified, to tell
     * QWizard to recheck the value stored by the mandatory field.
     * 
     * QWizard knows the most common Qt widgets. For these (or their
     * subclasses), you don't need to specify a <code>property</code> or a
     * <code>changedSignal</code>. The table below lists these widgets:
     * 
     * <code>QAbstractButton</code> (for which the relevant property is the
     * <code>checked</code> property), <code>QAbstractSlider</code> (the
     * <code>value</code> property), <code>QComboBox</code> (<code>currentIndex</code>
     * property), <code>QDateTimeEdit</code>(<code>dateTime</code>
     * property), <code>QLineEdit</code>(<code>text</code> property),
     * <code>QListWidget</code>(<code>currentRow</code> property), or
     * <code>QSpinBox</code>(<code>value</code> property).
     * 
     * @param name
     *            The name which will be used to access the field. Names ending
     *            with an asterisk signify mandatory fields.
     * @param widget
     *            The widget whose property will be accessed using this field.
     * @param property
     *            The name of the property associated with the field.
     * @param changedSignal
     *            The name of a signal which is emitted when the associated
     *            property's value changes.
     * 
     * @see com.trolltech.qt.gui.QWizardPage#field
     * @see com.trolltech.qt.gui.QWizardPage#setField
     */
    protected final void registerField(String name, QWidget widget, String property, String changedSignal) {
        String signalSignature = com.trolltech.qt.QtJambiInternal.cppSignalSignature(widget, changedSignal);
        if (signalSignature.length() == 0)
            throw new QNoSuchSignalException("Signal '" + changedSignal
                    + "' does not exist or has argument types that cannot be converted to Qt Jambi or java.lang types.");
        registerField(name, widget, com.trolltech.qt.QNativePointer.createCharPointer(property), com.trolltech.qt.QNativePointer
                .createCharPointer(com.trolltech.qt.QtJambiInternal.SignalPrefix + signalSignature));
    }

}// class

class QFontDialog___ extends QFontDialog {

    public static final class Result {
        public Result(QFont font, boolean ok) {
            this.font = font;
            this.ok = ok;
        }

        public QFont font;
        public boolean ok;
    }

}// class

class QMenu___ extends QMenu {

    protected final void initStyleOption(com.trolltech.qt.gui.QStyleOptionMenuItem option, QAction action) {
        initStyleOption(option.nativePointer(), action);
    }

}// class

class QMenuBar___ extends QMenuBar {

    protected final void initStyleOption(com.trolltech.qt.gui.QStyleOptionMenuItem option, QAction action) {
        initStyleOption(option.nativePointer(), action);
    }

}// class

class QPixmapCache___ extends QPixmapCache {

    public static boolean find(String key, QPixmap pm) {
        return find(key, pm.nativePointer());
    }

}// class

class QShortcut___ extends QShortcut {

    public QShortcut(QKeySequence key, QWidget parent) {
        this(key, parent, null, null, com.trolltech.qt.core.Qt.ShortcutContext.WindowShortcut);
    }

    public QShortcut(QKeySequence key, QWidget parent, com.trolltech.qt.core.Qt.ShortcutContext context) {
        this(key, parent, null, null, context);
    }

}// class

class QValidator___ extends QValidator {

    public static class QValidationData {
        public QValidationData(String input, int pos) {
            string = input;
            position = pos;
        }

        public String string;
        public int position;
    }

}// class

class QAbstractButton___ extends QAbstractButton {

    /**
     * Sets the shortcut to the key sequence for the given key string. For
     * example "Ctrl+O" gives CTRL+'O'. The strings "Ctrl", "Shift", "Alt" and
     * "Meta" are recognized, as well as their translated equivalents in the
     * "QShortcut" context (using QObject::tr()). Up to four key codes may be
     * entered by separating them with commas, e.g. "Alt+X,Ctrl+S,Q".
     * 
     * @param key
     *            The description of the key sequence. Typically used with tr()
     *            so key sequences can be locale aware.
     */
    public void setShortcut(String key) {
        setShortcut(new QKeySequence(key));
    }

    /**
     * Sets the shortcut to the key sequence for the given key. The result will
     * depend on the currently running platform. The key sequence will be based
     * on the first element in the list of key bindings for the key.
     * 
     * @param key
     *            The key for which to select a key sequence
     */
    public void setShortcut(QKeySequence.StandardKey key) {
        setShortcut(new QKeySequence(key));
    }

}// class

class QStyle___ extends QStyle {

    @QtBlockedSlot
    public final int combinedLayoutSpacing(QSizePolicy.ControlTypes controls1, QSizePolicy.ControlTypes controls2,
            com.trolltech.qt.core.Qt.Orientation orientation, QStyleOption option, QWidget widget) {
        return combinedLayoutSpacing(controls1, controls2, orientation, option.nativePointer(), widget);
    }

    @QtBlockedSlot
    public final int combinedLayoutSpacing(QSizePolicy.ControlTypes controls1, QSizePolicy.ControlTypes controls2,
            com.trolltech.qt.core.Qt.Orientation orientation, QStyleOption option) {
        return combinedLayoutSpacing(controls1, controls2, orientation, option, null);
    }

    @QtBlockedSlot
    public final int combinedLayoutSpacing(QSizePolicy.ControlTypes controls1, QSizePolicy.ControlTypes controls2,
            com.trolltech.qt.core.Qt.Orientation orientation) {
        return combinedLayoutSpacing(controls1, controls2, orientation, null);
    }

    @QtBlockedSlot
    public final int layoutSpacing(QSizePolicy.ControlType control1, QSizePolicy.ControlType control2, com.trolltech.qt.core.Qt.Orientation orientation,
            QStyleOption option, QWidget widget) {
        return layoutSpacing(control1, control2, orientation, option.nativePointer(), widget);
    }

    @QtBlockedSlot
    public final int layoutSpacing(QSizePolicy.ControlType control1, QSizePolicy.ControlType control2, com.trolltech.qt.core.Qt.Orientation orientation,
            QStyleOption option) {
        return layoutSpacing(control1, control2, orientation, option, null);
    }

    @QtBlockedSlot
    public final int layoutSpacing(QSizePolicy.ControlType control1, QSizePolicy.ControlType control2, com.trolltech.qt.core.Qt.Orientation orientation) {
        return layoutSpacing(control1, control2, orientation, null);
    }

    @QtBlockedSlot
    public final int layoutSpacingImplementation(QSizePolicy.ControlType control1, QSizePolicy.ControlType control2,
            com.trolltech.qt.core.Qt.Orientation orientation, QStyleOption option, QWidget widget) {
        return layoutSpacing(control1, control2, orientation, option.nativePointer(), widget);
    }

    @QtBlockedSlot
    public final int layoutSpacingImplementation(QSizePolicy.ControlType control1, QSizePolicy.ControlType control2,
            com.trolltech.qt.core.Qt.Orientation orientation, QStyleOption option) {
        return layoutSpacing(control1, control2, orientation, option, null);
    }

    @QtBlockedSlot
    public final int layoutSpacingImplementation(QSizePolicy.ControlType control1, QSizePolicy.ControlType control2,
            com.trolltech.qt.core.Qt.Orientation orientation) {
        return layoutSpacing(control1, control2, orientation, null);
    }

}// class

class QLayout___ extends QLayout {

    @QtBlockedSlot
    public final QContentsMargins getContentsMargins() {
        QNativePointer left = new QNativePointer(QNativePointer.Type.Int);
        QNativePointer top = new QNativePointer(QNativePointer.Type.Int);
        QNativePointer right = new QNativePointer(QNativePointer.Type.Int);
        QNativePointer bottom = new QNativePointer(QNativePointer.Type.Int);

        getContentsMargins(left, top, right, bottom);
        return new QContentsMargins(left.intValue(), top.intValue(), right.intValue(), bottom.intValue());
    }

    @QtBlockedSlot
    public final void setContentsMargins(QContentsMargins margins) {
        setContentsMargins(margins.left, margins.top, margins.right, margins.bottom);
    }

}// class

class QGridLayout___ extends QGridLayout {

    public final QTableArea getItemPosition(int index) {
        QNativePointer row = new QNativePointer(QNativePointer.Type.Int);
        QNativePointer column = new QNativePointer(QNativePointer.Type.Int);
        QNativePointer rowSpan = new QNativePointer(QNativePointer.Type.Int);
        QNativePointer columnSpan = new QNativePointer(QNativePointer.Type.Int);

        getItemPosition(index, row, column, rowSpan, columnSpan);

        return new QTableArea(row.intValue(), column.intValue(), rowSpan.intValue(), columnSpan.intValue());
    }

}// class

class QWidget___ extends QWidget {

    private native static void __qt_QMessageBox_setWindowTitle(long native_id, String windowTitle);

    private native static void __qt_QMessageBox_setWindowModality(long native_id, int modality);

    @QtBlockedSlot
    public final QContentsMargins getContentsMargins() {
        QNativePointer left = new QNativePointer(QNativePointer.Type.Int);
        QNativePointer top = new QNativePointer(QNativePointer.Type.Int);
        QNativePointer right = new QNativePointer(QNativePointer.Type.Int);
        QNativePointer bottom = new QNativePointer(QNativePointer.Type.Int);

        getContentsMargins(left, top, right, bottom);
        return new QContentsMargins(left.intValue(), top.intValue(), right.intValue(), bottom.intValue());
    }

    @QtBlockedSlot
    public final void setContentsMargins(QContentsMargins margins) {
        setContentsMargins(margins.left, margins.top, margins.right, margins.bottom);
    }

}// class

class QFileDialog___ extends QFileDialog {

    public static class Filter {
        public Filter(String filter) {
            this.filter = filter;
        }

        public String filter;
        public String selectedFilter = "";
    };

}// class

class QTabBar___ extends QTabBar {

    public final void initStyleOption(QStyleOptionTab option, int tabIndex) {
        initStyleOption(option.nativePointer(), tabIndex);
    }

}// class

class QClipboard___ extends QClipboard {

    public static class Text {
        public String text;
        public String subtype;
    }

    public final Text text(String subtype, Mode mode) {
        QNativePointer np = new QNativePointer(QNativePointer.Type.String);
        np.setStringValue(subtype != null ? subtype : "");

        Text returned = new Text();
        returned.text = text(np, mode);
        returned.subtype = np.stringValue();
        return returned;
    }

    public final Text text(String subtype) {
        return text(subtype, Mode.Clipboard);
    }

}// class

class QAbstractScrollArea___ extends QAbstractScrollArea {

    public QPaintEngine paintEngine() {
        throw new RuntimeException("Cannot open a painter directly on a QAbstractScrollArea, open QPainter on its viewport instead...");
    }

}// class

class QTextDocument___ extends QTextDocument {

    public final void redo(QTextCursor cursor) {
        redo(cursor.nativePointer());
    }

    public final void undo(QTextCursor cursor) {
        undo(cursor.nativePointer());
    }

}// class

class QSplitter___ extends QSplitter {

    public static class Range {
        public Range(int min, int max) {
            minimum = min;
            maximum = max;
        }

        public int minimum;
        public int maximum;
    }

    public Range getRange(int index) {
        QNativePointer min = new QNativePointer(QNativePointer.Type.Int);
        QNativePointer max = new QNativePointer(QNativePointer.Type.Int);

        getRange(index, min, max);

        return new Range(min.intValue(), max.intValue());
    }

}// class

class QAction___ extends QAction {

    private QActionGroup __rcActionGroup = null;

    /**
     * Sets the shortcut to the key sequence for the given key string. For
     * example "Ctrl+O" gives CTRL+'O'. The strings "Ctrl", "Shift", "Alt" and
     * "Meta" are recognized, as well as their translated equivalents in the
     * "QShortcut" context (using QObject::tr()). Up to four key codes may be
     * entered by separating them with commas, e.g. "Alt+X,Ctrl+S,Q".
     * 
     * @param key
     *            The description of the key sequence. Typically used with tr()
     *            so key sequences can be locale aware.
     */
    public void setShortcut(String key) {
        setShortcut(new QKeySequence(key));
    }

    /**
     * Sets the shortcut to the key sequence for the given key. The result will
     * depend on the currently running platform. The key sequence will be based
     * on the first element in the list of key bindings for the key.
     * 
     * @param key
     *            The key for which to select a key sequence
     */
    public void setShortcut(QKeySequence.StandardKey key) {
        setShortcut(new QKeySequence(key));
    }

    public void setIcon(QPixmap pm) {
        setIcon(new QIcon(pm));
    }

}// class

class QPainter___ extends QPainter {

    public void setBrush(QColor color) {
        setBrush(new QBrush(color));
    }

    public void setBrush(QGradient gradient) {
        setBrush(new QBrush(gradient));
    }

    public void setBrush(QPixmap pm) {
        setBrush(new QBrush(pm));
    }

    public static QPaintDeviceInterface redirected(QPaintDeviceInterface device, com.trolltech.qt.core.QPoint offset) {
        return redirected(device, offset == null ? null : offset.nativePointer());
    }

    public QPainter(QWidget widget) {
        this();
        begin(widget);
    }

    public boolean begin(QWidget widget) {
        return com.trolltech.qt.QtJambiGuiInternal.beginPaint(widget, this);
    }

    private static java.util.Stack<QPaintDeviceInterface> __rcRedirections = new java.util.Stack<QPaintDeviceInterface>();

}// class

class QApplication___ extends QApplication {

    public static void initialize(String args[]) {
        if (m_instance != null)
            throw new RuntimeException("QApplication can only be initialized once");

        m_instance = new QApplication(args);
        m_instance.aboutToQuit.connect(m_instance, "disposeOfMyself()");
        String path = Utilities.unpackPlugins();
        if (path != null)
            addLibraryPath(path);
        else
            QtJambiInternal.setupDefaultPluginPath();
    }

    public static void aboutQtJambi() {
        com.trolltech.qt.QtJambiGuiInternal.aboutQtJambi();
    }

    public static QApplication instance() {
        if (type() != Type.Tty)
            return (QApplication) com.trolltech.qt.core.QCoreApplication.instance();
        return null;
    }

    public QApplication(String args[]) {
        this(argc(args), argv(args));
    }

    public static void setFont(QFont font) {
        setFont(font, null);
    }

    public static void setPalette(QPalette palette) {
        setPalette(palette, null);
    }

    public static QCursor overrideCursor() {
        QNativePointer np = overrideCursor_private();
        return np == null ? null : QCursor.fromNativePointer(np);
    }

}// class
