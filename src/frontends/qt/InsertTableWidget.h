// -*- C++ -*-
/**
 * \file InsertTableWidget.h
 * This file is part of LyX, the document processor.
 * Licence details can be found in the file COPYING.
 *
 * \author Edwin Leuven
 *
 * Full author contact details are available in file CREDITS.
 */


#ifndef INSERTTABLEWIDGET_H
#define INSERTTABLEWIDGET_H

#include <QWidget>
#include <QProxyStyle>

namespace lyx {
namespace frontend {

class GuiView;

// A proxy style to get rif of the style-specific tool tip delay
// (https://forum.qt.io/topic/90403/show-tooltip-immediatly/6)
class ProxyStyle : public QProxyStyle
{
public:
	using QProxyStyle::QProxyStyle;
	int styleHint(StyleHint hint, const QStyleOption * option = nullptr,
		      const QWidget* widget = nullptr,
		      QStyleHintReturn* returnData = nullptr) const override
	{
		if (hint == QStyle::SH_ToolTip_WakeUpDelay)
			return 0;
		else if (hint == QStyle::SH_ToolTip_FallAsleepDelay)
			return 0;
		return QProxyStyle::styleHint(hint, option, widget, returnData);
	}
};

class InsertTableWidget : public QWidget {
	Q_OBJECT
public:

	InsertTableWidget(QWidget *);

Q_SIGNALS:
	//! widget is visible
	void visible(bool);

public Q_SLOTS:
	//! show the widget
	void show(bool);
	//! enable/disable parent
	void updateParent();

protected Q_SLOTS:
	void mouseMoveEvent(QMouseEvent *) override;
	void mouseReleaseEvent(QMouseEvent *) override;
	void mousePressEvent(QMouseEvent *) override;
	void paintEvent(QPaintEvent *) override;
	void hideEvent(QHideEvent * event) override;

private:
	//! update the geometry
	void resetGeometry();
	//! initialize parameters to default values
	void init();
	//! draw the grid
	void drawGrid(int rows, int cols, QBrush fillBrush, QColor lineColor);

	//! colwidth in pixels
	int colwidth_;
	//! rowheight in pixels
	int rowheight_;
	//! total rows
	int rows_;
	//! minimum number of rows
	int minrows_;
	//! total cols
	int cols_;
	//! minimum number of cols
	int mincols_;
	//! row of pointer
	int bottom_;
	//! column of pointer
	int right_;
	//! widget under mouse
	bool underMouse_;
};

} // namespace frontend
} // namespace lyx

#endif // INSERTTABLEWIDGET_H
