------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                          KICAD V6 / SCHEMATIC                            --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
-- Copyright (C) 2017 - 2026                                                --
-- Mario Blunk / Blunk electronic                                           --
-- Buchfinkenweg 3 / 99097 Erfurt / Germany                                 --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
------------------------------------------------------------------------------

--   For correct displaying set tab width in your editor to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

-- DESCRIPTION:
--
--   The KiCad v6 schematic model: everything a .kicad_sch file (or a whole
--   hierarchical project of them) can contain, plus the hierarchical
--   project loader (import_design) and per-sheet reader (read_sheet_file).
--
--   This is a LOADER, not a renderer: symbol body graphics (polylines,
--   arcs, circles, rectangles) are kept as opaque parsed sub-trees rather
--   than being modeled field by field -- only pins (which matter for
--   connectivity) are modeled with full fidelity.
--
--   Net/strand connectivity (which wire/bus segments belong together, and
--   which label names a given strand) is built HERE, during parsing --
--   not deferred to native conversion. This mirrors et_kicad.schematic's
--   own v4 architecture (see et_kicad-schematic.ads, type_strand/add_strand)
--   where the same separation of concerns applies.
--
--   history of changes:
--

with ada.containers.doubly_linked_lists;
with ada.containers.vectors;
with ada.containers.ordered_maps;

with et_bounded_string_helpers;
with et_schematic_geometry;		use et_schematic_geometry;
with et_mirroring;					use et_mirroring;
with et_project_name;

with et_kicad_v6.sexp;

package et_kicad_v6.schematic is

	use pac_geometry_2;


	------------------------------------------------------------------
	-- BOUNDED TEXT / PROPERTY TYPES
	------------------------------------------------------------------

	-- The name half of a KiCad (property "Name" "Value" ...) pair,
	-- e.g. "Reference", "Value", "Footprint", or an arbitrary
	-- project-specific name such as "Location":
	property_name_length_max : constant natural := 100;
	package pac_property_name is new generic_bounded_length (property_name_length_max);
	type type_property_name is new pac_property_name.bounded_string;

	function to_string is new et_bounded_string_helpers.to_string   (pac_property_name, from_type => type_property_name);
	function to_property_name is new et_bounded_string_helpers.from_string (pac_property_name, to_type => type_property_name);


	-- The value half of a property, and also used generically for
	-- any other free-form text field (label/text content, sheet
	-- names and file names, titles, ...). Sized generously: the
	-- longest single quoted atom seen in practice is a multi-line
	-- (text ...) block:
	property_value_length_max : constant natural := 2000;
	package pac_property_value is new generic_bounded_length (property_value_length_max);
	type type_property_value is new pac_property_value.bounded_string;

	function to_string is new et_bounded_string_helpers.to_string   (pac_property_value, from_type => type_property_value);
	function to_property_value is new et_bounded_string_helpers.from_string (pac_property_value, to_type => type_property_value);


	package pac_properties is new ada.containers.ordered_maps
		(key_type => type_property_name, element_type => type_property_value);


	-- A raw "library:symbol" reference, e.g. "r1000:F37":
	lib_id_length_max : constant natural := 300;
	package pac_lib_id is new generic_bounded_length (lib_id_length_max);
	type type_lib_id is new pac_lib_id.bounded_string;

	function to_string is new et_bounded_string_helpers.to_string   (pac_lib_id, from_type => type_lib_id);
	function to_lib_id is new et_bounded_string_helpers.from_string (pac_lib_id, to_type   => type_lib_id);

	-- The part of a lib_id before/after the ':'. Returns the whole
	-- string if no ':' is present (defensive -- not expected):
	function library_of (id : in type_lib_id) return string;
	function symbol_of  (id : in type_lib_id) return string;


	------------------------------------------------------------------
	-- PINS, SUB-UNITS, LIB_SYMBOLS (the embedded symbol library)
	------------------------------------------------------------------

	-- KiCad's full pin electrical-type vocabulary. Only a subset is
	-- exercised by any one project -- see et_kicad_v6_to_native for
	-- the (necessarily lossy in a couple of cases) mapping onto
	-- et_port_direction.type_port_direction:
	type type_pin_electrical_type is (
		PIN_INPUT, PIN_OUTPUT, PIN_BIDIRECTIONAL, PIN_TRI_STATE,
		PIN_PASSIVE, PIN_FREE, PIN_UNSPECIFIED,
		PIN_POWER_IN, PIN_POWER_OUT,
		PIN_OPEN_COLLECTOR, PIN_OPEN_EMITTER, PIN_NO_CONNECT);

	function to_pin_electrical_type (text : in string) return type_pin_electrical_type;

	-- Rendering-only; kept because the grammar has it, not used by
	-- anything downstream of this loader:
	type type_pin_graphic_style is (
		STYLE_LINE, STYLE_INVERTED, STYLE_CLOCK, STYLE_INVERTED_CLOCK,
		STYLE_INPUT_LOW, STYLE_CLOCK_LOW, STYLE_OUTPUT_LOW,
		STYLE_EDGE_CLOCK_HIGH, STYLE_NON_LOGIC);

	function to_pin_graphic_style (text : in string) return type_pin_graphic_style;


	type type_pin is record
		name, number	: type_property_value; -- both textual: numbers can be "A1" etc.
		uuid			: type_uuid;
		position		: type_vector_model; -- relative to the owning sub-unit's local origin
		length			: type_distance_model := 0.0;
		orientation		: type_rotation_model := 0.0;
		electrical_type	: type_pin_electrical_type := PIN_UNSPECIFIED;
		graphic_style	: type_pin_graphic_style := STYLE_LINE;
	end record;

	package pac_pins is new ada.containers.doubly_linked_lists (type_pin);


	-- One (unit, convert) sub-symbol as named "<NAME>_<unit>_<convert>".
	-- unit 0 means "graphics common to all units"; convert 1 is the
	-- normal body, convert 2 the alternate/DeMorgan body:
	type type_symbol_unit_key is record
		unit, convert : natural := 0;
	end record;

	-- Graphics are kept as an opaque, verbatim parsed sub-tree --
	-- this is a loader, not a renderer. kind is recorded only so a
	-- future consumer can filter without re-parsing the sub-tree:
	type type_symbol_graphic_item_kind is
		(GFX_POLYLINE, GFX_ARC, GFX_CIRCLE, GFX_RECTANGLE, GFX_TEXT, GFX_OTHER);

	type type_symbol_graphic_item is record
		item_kind	: type_symbol_graphic_item_kind;
		raw			: et_kicad_v6.sexp.type_node_access;
	end record;

	package pac_symbol_graphics is new ada.containers.doubly_linked_lists (type_symbol_graphic_item);

	type type_symbol_sub_unit is record
		key			: type_symbol_unit_key;
		pins		: pac_pins.list;
		graphics	: pac_symbol_graphics.list;
	end record;

	package pac_symbol_sub_units is new ada.containers.doubly_linked_lists (type_symbol_sub_unit);


	type type_lib_symbol is record
		lib_id					: type_lib_id;
		is_power				: boolean := false;
		pin_numbers_hidden		: boolean := false;
		pin_names_hidden		: boolean := false;
		in_bom, on_board		: boolean := true;
		properties				: pac_properties.map; -- Reference/Value/Footprint/Datasheet/custom template
		sub_units				: pac_symbol_sub_units.list;
	end record;

	package pac_lib_symbols is new ada.containers.ordered_maps (type_lib_id, type_lib_symbol);

	-- Returns the sub-unit matching (unit, convert), falling back to
	-- (unit, 1) and then (0, 1) [graphics/pins common to all units]
	-- if an exact match is not found. Returns a sub-unit with empty
	-- pins/graphics (not an exception) if the symbol has no match at
	-- all -- callers can check pins.is_empty on the result:
	function get_sub_unit (
		symbol			: in type_lib_symbol;
		unit, convert	: in natural) return type_symbol_sub_unit;


	------------------------------------------------------------------
	-- PLACED SYMBOLS (component instances)
	------------------------------------------------------------------

	-- One (instances (project P (path "..." (reference ..) ...))))
	-- entry: the same symbol placement can carry a different
	-- reference/value/footprint per project instance of the sheet
	-- it sits on:
	type type_instance_ref is record
		project_name	: type_property_value;
		path			: pac_uuid_path.list; -- sheet-uuid path from root to the sheet CONTAINING this symbol
		reference		: type_property_value;
		value			: type_property_value;
		footprint		: type_property_value;
		unit			: positive := 1;
	end record;

	package pac_instance_refs is new ada.containers.doubly_linked_lists (type_instance_ref);

	type type_placed_symbol is record
		lib_id			: type_lib_id;
		position		: type_vector_model;
		orientation		: type_rotation_model := 0.0;
		mirror			: type_mirror := MIRROR_NO;
		unit, convert	: positive := 1;
		in_bom, on_board : boolean := true;
		dnp				: boolean := false;
		uuid			: type_uuid;
		properties		: pac_properties.map;
		instances		: pac_instance_refs.list;
	end record;

	package pac_placed_symbols is new ada.containers.doubly_linked_lists (type_placed_symbol);


	------------------------------------------------------------------
	-- WIRES, BUSES, BUS ENTRIES, JUNCTIONS, NO-CONNECTS
	------------------------------------------------------------------

	package pac_points is new ada.containers.vectors (positive, type_vector_model);
	-- (pts ...) is modeled as an N-point vector, not a fixed 2-point
	-- pair -- the grammar allows polylines with more than 2 points
	-- even though every wire/bus segment sampled from the reference
	-- project happens to be exactly 2 points.

	type type_wire is record
		points	: pac_points.vector;
		uuid	: type_uuid;
	end record;

	type type_bus is record
		points	: pac_points.vector;
		uuid	: type_uuid;
	end record;

	type type_bus_entry is record
		position, size	: type_vector_model;
		uuid			: type_uuid;
	end record;

	type type_junction is record
		position	: type_vector_model;
		diameter	: type_distance_model := 0.0;
		uuid		: type_uuid;
	end record;

	type type_no_connect is record
		position	: type_vector_model;
		uuid		: type_uuid;
	end record;

	package pac_wires		is new ada.containers.doubly_linked_lists (type_wire);
	package pac_buses		is new ada.containers.doubly_linked_lists (type_bus);
	package pac_bus_entries is new ada.containers.doubly_linked_lists (type_bus_entry);
	package pac_junctions	is new ada.containers.doubly_linked_lists (type_junction);
	package pac_no_connects is new ada.containers.doubly_linked_lists (type_no_connect);


	------------------------------------------------------------------
	-- LABELS AND FREE TEXT
	------------------------------------------------------------------

	type type_label_kind is (LABEL_LOCAL, LABEL_GLOBAL, LABEL_HIERARCHICAL);

	-- NOT the same vocabulary as type_pin_electrical_type -- label
	-- shape only ever names a subset of it (a label is never, e.g.,
	-- "power_in") -- kept as its own small enum rather than reusing
	-- either existing one:
	type type_label_shape is (SHAPE_INPUT, SHAPE_OUTPUT, SHAPE_BIDIRECTIONAL, SHAPE_TRI_STATE, SHAPE_PASSIVE);

	function to_label_shape (text : in string) return type_label_shape;

	-- One unified record for local/global/hierarchical labels: they
	-- differ by exactly one optional field (shape, meaningless for
	-- LABEL_LOCAL), so a 3-way variant would add ceremony for no
	-- real benefit -- the dominant downstream operation ("all
	-- labels on this sheet") wants one flat list regardless of kind:
	type type_label is record
		label_kind	: type_label_kind;
		text		: type_property_value;
		position	: type_vector_model;
		orientation	: type_rotation_model := 0.0;
		shape		: type_label_shape := SHAPE_PASSIVE; -- meaningful for GLOBAL/HIERARCHICAL only
		uuid		: type_uuid;
	end record;

	package pac_labels is new ada.containers.doubly_linked_lists (type_label);


	type type_free_text is record
		text		: type_property_value;
		position	: type_vector_model;
		orientation	: type_rotation_model := 0.0;
		uuid		: type_uuid;
	end record;

	package pac_free_texts is new ada.containers.doubly_linked_lists (type_free_text);


	------------------------------------------------------------------
	-- STRAND / NET CONNECTIVITY
	--
	-- Built from a sheet's own wires/buses/junctions/labels once
	-- read_sheet_file has parsed them (see et_kicad_v6-schematic-
	-- strands.adb). A strand is a group of wire/bus segments joined
	-- by shared endpoint coordinates (optionally confirmed by an
	-- explicit junction where 3+ segments meet at one point).
	--
	-- CS: connectivity is currently endpoint-coordinate matching
	-- only. A junction sitting at the midpoint of another segment's
	-- run (a "true" T-touch, without any segment endpoint coinciding
	-- there) is NOT yet detected as a connection -- every junction
	-- in the reference project this was built against coincides
	-- with a segment endpoint, so this gap has not been exercised.
	-- log_unknown_key-style logging flags the situation (a
	-- SEVERITY_WARNING) if such a junction is ever encountered, so
	-- the gap stays visible rather than silently under-connecting.
	------------------------------------------------------------------

	type type_strand is record
		points		: pac_points.vector;   -- every distinct coordinate belonging to this strand
		labels		: pac_labels.list;      -- every label (of any kind) attached to this strand
	end record;

	package pac_strands is new ada.containers.doubly_linked_lists (type_strand);

	-- A net is a name (from a GLOBAL label; project-wide scope) plus
	-- every strand -- possibly on different sheets -- carrying that
	-- name. Strands with no global label stay un-merged, sheet-local,
	-- reachable only via type_sheet_data.strands, not via this map:
	package pac_nets is new ada.containers.ordered_maps
		(key_type => type_property_value, element_type => pac_strands.list, "=" => pac_strands."=");


	------------------------------------------------------------------
	-- SHEET HIERARCHY: one (sheet ...) reference block, and the
	-- content of one physical file
	------------------------------------------------------------------

	-- CS: STUB. No (pin ...) inside a (sheet ...) block was observed
	-- in the reference project (its cross-sheet connectivity is done
	-- entirely via global_labels instead), but the grammar supports
	-- hierarchical sheet-pins. If one is ever encountered, it is
	-- logged (et_kicad_v6.log_unknown_key, deferred => true) rather
	-- than silently dropped -- see read_sheet_file's body.
	type type_sheet_pin is record
		name		: type_property_value;
		position	: type_vector_model;
		orientation	: type_rotation_model := 0.0;
		shape		: type_label_shape := SHAPE_PASSIVE;
		uuid		: type_uuid;
	end record;

	package pac_sheet_pins is new ada.containers.doubly_linked_lists (type_sheet_pin);

	type type_sheet_ref is record
		uuid				: type_uuid;
		position, size		: type_vector_model;
		sheetname			: type_property_value;
		sheetfile			: type_property_value;
		pins				: pac_sheet_pins.list; -- CS stub, see above
	end record;

	package pac_sheet_refs is new ada.containers.doubly_linked_lists (type_sheet_ref);


	-- CS: STUB. No (bus_alias ...) observed in the reference
	-- project; grammar supports it, room reserved -- see
	-- read_sheet_file's body for the log_unknown_key (deferred)
	-- notice this triggers if one is ever encountered.
	package pac_property_value_list is new ada.containers.doubly_linked_lists (type_property_value);

	type type_bus_alias is record
		name	: type_property_value;
		members	: pac_property_value_list.list;
	end record;

	package pac_bus_aliases is new ada.containers.doubly_linked_lists (type_bus_alias);


	-- The content of ONE physical .kicad_sch file, shared across all
	-- of its instances in the tree if the same file is referenced
	-- from more than one parent sheet (see type_project.file_cache):
	type type_sheet_data is record
		file_path		: type_property_value; -- resolved/normalized absolute path; the cache key
		version			: natural := 0;
		generator		: type_property_value;
		uuid			: type_uuid;
		paper_size		: type_property_value;
		paper_width		: type_distance_model := 0.0;
		paper_height	: type_distance_model := 0.0;
		title			: type_property_value; -- CS: date/rev/company/comment of title_block not yet populated

		symbols			: pac_lib_symbols.map;    -- this file's own embedded lib_symbols
		child_sheets	: pac_sheet_refs.list;
		placed_symbols	: pac_placed_symbols.list;
		wires			: pac_wires.list;
		buses			: pac_buses.list;
		bus_entries		: pac_bus_entries.list;
		junctions		: pac_junctions.list;
		no_connects		: pac_no_connects.list;
		labels			: pac_labels.list;
		texts			: pac_free_texts.list;
		bus_aliases		: pac_bus_aliases.list;    -- CS stub, see above

		strands			: pac_strands.list; -- built from the fields above, see et_kicad_v6.schematic.strands
	end record;

	type type_sheet_data_access is access type_sheet_data;


	------------------------------------------------------------------
	-- THE HIERARCHY: one instance of a sheet in the tree, and the
	-- whole loaded project
	------------------------------------------------------------------

	type type_sheet_node;
	type type_sheet_node_access is access type_sheet_node;

	package pac_sheet_node_children is new ada.containers.vectors (positive, type_sheet_node_access);

	type type_sheet_node is record
		uuid_path	: pac_uuid_path.list;      -- full path from root to (and including) this instance's own uuid
		data		: type_sheet_data_access;  -- shared; may be pointed to by >1 node if a subsheet is reused
		parent		: type_sheet_node_access;  -- null for the root
		children	: pac_sheet_node_children.vector;
		page		: type_property_value;     -- from root's (sheet_instances ...); "" if unresolved
	end record;

	package pac_sheet_data_by_path is new ada.containers.ordered_maps (type_property_value, type_sheet_data_access);

	type type_project is record
		name			: et_project_name.type_project_name;
		root			: type_sheet_node_access;
		merged_symbols	: pac_lib_symbols.map; -- project-wide fold, first-definition-wins
		merged_nets		: pac_nets.map;         -- project-wide fold of every global-label-named strand group
		file_cache		: pac_sheet_data_by_path.map;
	end record;


	-- Raised by import_design when a sheet is found to be its own
	-- ancestor in the hierarchy (a genuine cyclic reference). The
	-- partially-built tree is still returned by import_design's
	-- caller-visible result rather than discarded -- see the
	-- project plan's error-handling philosophy:
	sheet_cycle_error : exception;


	------------------------------------------------------------------
	-- ENTRY POINTS
	------------------------------------------------------------------

	-- Parses exactly one physical .kicad_sch file (no hierarchy
	-- traversal -- see import_design for that) into a
	-- type_sheet_data, including building that sheet's own strands.
	-- file_path may be relative or absolute; it is used verbatim as
	-- the returned record's file_path field is instead the resolved
	-- absolute path (see the body).
	function read_sheet_file (
		file_path		: in string;
		log_threshold	: in type_log_level)
		return type_sheet_data;

	-- Loads project (a project name, e.g. "SEQ") by opening
	-- "<project>.kicad_sch" in project_directory, and recursively
	-- loading every referenced sheet, building the full hierarchical
	-- tree, resolving each sheet instance's page number from the
	-- root's (sheet_instances ...) block, and folding every sheet's
	-- symbols/named strands into the returned type_project's
	-- project-wide maps.
	function import_design (
		project				: in et_project_name.type_project_name;
		project_directory	: in string;
		log_threshold		: in type_log_level)
		return type_project;


end et_kicad_v6.schematic;

-- Soli Deo Gloria

-- For God so loved the world that he gave
-- his one and only Son, that whoever believes in him
-- shall not perish but have eternal life.
-- The Bible, John 3.16
