use svg::node::{
    element::{Circle, Definitions, Group, Path, Style, Use, SVG},
    Node,
};

#[derive(Debug)]
pub struct Document {
    view_box: (f64, f64, f64, f64),
    stylesheets: Vec<String>,
    routes_def: Definitions,
    stops_def: Definitions,
    routes_use: Group,
    stops_use: Group,
}

impl Document {
    pub fn new() -> Document {
        Document::default()
    }

    pub fn add_route(&mut self, id: &str, style: &str, path: Path) {
        self.routes_def.append(path);
        self.routes_use.append(
            Use::new()
                .set("href", format!("#route-{}", id))
                .set("class", format!("route bg {} {}", id, style)),
        );
        self.routes_use.append(
            Use::new()
                .set("href", format!("#route-{}", id))
                .set("class", format!("route mg {} {}", id, style)),
        );
        self.routes_use.append(
            Use::new()
                .set("href", format!("#route-{}", id))
                .set("class", format!("route fg {} {}", id, style)),
        );
    }

    pub fn add_stop(&mut self, stop: Use) {
        self.stops_use.append(stop);
    }

    pub fn set_view_box(&mut self, top: f64, left: f64, bottom: f64, right: f64) {
        self.view_box = (left, top, right - left, bottom - top)
    }

    pub fn add_stylesheets(&mut self, stylesheets: &[String]) {
        self.stylesheets.extend(stylesheets.iter().cloned())
    }

    /// Compiles the document to an SVG element
    pub fn compile(self) -> SVG {
        let style_content = self
            .stylesheets
            .into_iter()
            .map(|s| format!("@import url({})\n;", s))
            .collect::<String>();
        SVG::new()
            .set("viewBox", self.view_box)
            .set("height", self.view_box.3)
            .set("width", self.view_box.2)
            .add(Style::new(style_content))
            .add(self.routes_def)
            .add(self.stops_def)
            .add(self.routes_use)
            .add(self.stops_use)
    }
}

impl Default for Document {
    fn default() -> Document {
        Document {
            view_box: Default::default(),
            stylesheets: Default::default(),
            routes_def: Definitions::new(),
            stops_def: Definitions::new().add(Circle::new().set("id", "stop")),
            routes_use: Group::new().set("id", "routes"),
            stops_use: Group::new().set("id", "stops"),
        }
    }
}
