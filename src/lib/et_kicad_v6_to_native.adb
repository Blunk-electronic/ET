------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                     KICAD V6 TO NATIVE CONVERSION                        --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
-- Copyright (C) 2026 Jesper Quorning                                       --
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

with ada.directories;					use ada.directories;
with ada.strings;						use ada.strings;
with ada.strings.fixed;					use ada.strings.fixed;
with ada.strings.unbounded;
with ada.characters.handling;
with ada.containers;						use ada.containers;
with ada.containers.ordered_sets;
with ada.containers.ordered_maps;

with et_kicad_v6;						use et_kicad_v6;

with et_project_name;					use et_project_name;
with et_project;
with ada.calendar;						use ada.calendar;
with et_time;
with et_meta;
with et_module_names;					use et_module_names;
with et_module_write;
with et_schematic_text;					use et_schematic_text;
with et_generic_modules;					use et_generic_modules;
with et_alignment;						use et_alignment;

with et_device_appearance;				use et_device_appearance;
with et_device_library;					use et_device_library;
with et_device_model;					use et_device_model;
with et_device_model_names;				use et_device_model_names;
with et_device_model_unit_internal;		use et_device_model_unit_internal;
with et_device_model_unit_external;		use et_device_model_unit_external;
with et_device_prefix;
with et_device_value;
with et_device_partcode;
with et_device_purpose;
with et_device_name;
with et_devices_electrical;				use et_devices_electrical;
with et_units;							use et_units;
with et_unit_name;
with et_device_placeholders;			use et_device_placeholders;
with et_device_placeholders.symbols;	use et_device_placeholders.symbols;
with et_object_status;					use et_object_status;
with et_device_write;
with et_package_write;

with et_symbol_model;					use et_symbol_model;
with et_symbol_shapes;					use et_symbol_shapes;
with et_directions;						use et_directions;
with et_kicad_v6.sexp;					use et_kicad_v6.sexp;
with et_symbol_text;
with et_symbol_ports;					use et_symbol_ports;
with et_symbol_port_general;				use et_symbol_port_general;
with et_symbol_port_measures;				use et_symbol_port_measures;
with et_port_names;
with et_port_direction;					use et_port_direction;
with et_port_strength;					use et_port_strength;
with et_logic;							use et_logic;
with et_port_sensitivity;				use et_port_sensitivity;
with et_power_sources;					use et_power_sources;

with et_package_variant_name;
with et_package_variant;					use et_package_variant;
with et_package_library;					use et_package_library;
with et_package_bom_relevance;			use et_package_bom_relevance;
with et_package_model_name;
with et_board_coordinates;

with et_schematic_geometry;				use et_schematic_geometry;
with et_schematic_coordinates;			use et_schematic_coordinates;
with et_sheets;							use et_sheets;
with et_drawing_frame.schematic;
with et_text_content;

with et_nets;
with et_net_strands;
with et_net_segment;						use et_net_segment;
with et_net_labels;						use et_net_labels;
with et_rotation_docu;					use et_rotation_docu;
with et_net_names;						use et_net_names;
with et_net_scope;						use et_net_scope;

package body et_kicad_v6_to_native is

	-- NOTE: et_nets/et_net_strands are deliberately NOT use-visible here
	-- (only plain with'd, referenced fully qualified below) -- their
	-- pac_nets/type_strand names collide with et_kicad_v6.schematic's
	-- own pac_nets/type_strand (the KiCad-side connectivity model),
	-- and having both use-visible at once makes every such name
	-- ambiguous. Likewise pac_points (et_schematic_geometry's generic
	-- point-list vs. et_kicad_v6.schematic's own) -- pac_geometry_2 is
	-- therefore not use'd either; nothing here needs it by name.
	use et_kicad_v6.schematic;
	use et_kicad_v6.pac_uuid_path;

	use et_device_library.pac_device_models;

	-- type_package_variant_name / type_device_value only allow
	-- letters/digits/'_'/'-' -- no '/', unlike
	-- et_device_partcode.partcode_default ("N/A"):
	placeholder_not_assigned : constant string := "not_assigned";

	placeholder_package_name : constant et_package_model_name.type_package_model_name :=
		et_package_model_name.to_package_model_name (
			compose (compose (et_project.directory_libraries, et_project.directory_libraries_packages),
				placeholder_not_assigned));

	-- Every synthesized PCB-appearance device model gets exactly one
	-- package variant, named placeholder_not_assigned, pointing to a
	-- single shared placeholder package model with no terminals --
	-- there is no footprint data in this KiCad schematic-only project
	-- (out of scope, see the package spec), but
	-- et_module_read_device_electrical requires a device's variant
	-- name to resolve to a real entry in its model's variants map,
	-- which in turn requires a real (if empty) package model to
	-- exist in the rig-wide package_library:
	function placeholder_variants return pac_package_variants.map is
		package_cursor : pac_package_models.cursor;
		variants		: pac_package_variants.map;
		variant_cursor	: pac_package_variants.cursor;
		inserted		: boolean;
	begin
		if not et_package_library.pac_package_models.contains (package_library, placeholder_package_name) then
			create_package (
				package_name	=> placeholder_package_name,
				appearance		=> BOM_RELEVANT_NO,
				log_threshold	=> 0);
		end if;

		package_cursor := get_package_model (placeholder_package_name);

		pac_package_variants.insert (
			container	=> variants,
			key			=> et_package_variant_name.to_variant_name (placeholder_not_assigned),
			position	=> variant_cursor,
			inserted	=> inserted,
			new_item	=> (model_cursor => package_cursor, others => <>));

		return variants;
	end placeholder_variants;


	------------------------------------------------------------------
	-- HELPERS THAT NEED NO OUTER STATE
	------------------------------------------------------------------

	-- Reads the "Reference" property of a lib_symbol (e.g. "U", "R",
	-- "#PWR") and reduces it to an ET device prefix: leading '#'
	-- dropped, upper-cased, cut off at the first digit or '?' (KiCad
	-- symbol library Reference properties normally carry no digits at
	-- all -- the cut-off is defensive, not expected to trigger):
	function extract_prefix (
		props : in pac_properties.map)
		return et_device_prefix.type_device_prefix
	is
		use pac_properties;
		use ada.strings.unbounded;
		use ada.characters.handling;

		c   : constant pac_properties.cursor := find (props, to_property_name ("Reference"));
		raw : constant string := (if c /= pac_properties.no_element then to_string (element (c)) else "U");
		buf : unbounded_string;
	begin
		for ch of raw loop
			exit when ch in '0' .. '9' or ch = '?';

			if ch /= '#' then
				append (buf, to_upper (ch));
			end if;
		end loop;

		if length (buf) = 0 then
			return et_device_prefix.to_prefix ("U");
		else
			return et_device_prefix.to_prefix (to_string (buf));
		end if;
	end extract_prefix;


	-- Sanitizes a raw string (a KiCad "Value" property, whether from a
	-- lib_symbol's library default or a placed symbol's per-instance
	-- override) to et_device_value's allowed character set
	-- (letters/digits/'_'/'-' only, max value_length_max chars).
	-- KiCad values are usually already conformant, but this must not
	-- silently produce a multi-field *.dev/*.mod line (e.g. a value
	-- containing a space) or an empty one where the caller writes
	-- "value <text>" with a mandatory argument:
	function sanitize_device_value (
		raw : in string)
		return et_device_value.type_device_value
	is
		use ada.strings.unbounded;
		buf : unbounded_string;
	begin
		for ch of raw loop
			exit when length (buf) = et_device_value.value_length_max;

			if ch in 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '-' then
				append (buf, ch);
			end if;
		end loop;

		if length (buf) = 0 then
			return et_device_value.to_value (placeholder_not_assigned);
		else
			return et_device_value.to_value (to_string (buf));
		end if;
	end sanitize_device_value;


	-- Reads the "Value" property of a lib_symbol (its library default,
	-- e.g. "F00", "74LS00", "10k") and sanitizes it -- see
	-- sanitize_device_value:
	function extract_value (
		props : in pac_properties.map)
		return et_device_value.type_device_value
	is
		use pac_properties;
		c : constant pac_properties.cursor := find (props, to_property_name ("Value"));
	begin
		if c /= pac_properties.no_element then
			return sanitize_device_value (to_string (element (c)));
		else
			return sanitize_device_value ("");
		end if;
	end extract_value;


	-- Reads the "Name" property of a PLACED symbol -- this project's
	-- own convention (not part of the KiCad file format) for a
	-- per-instance functional label distinct from "Value": many
	-- instances share one lib_symbol/Value (e.g. several gates all
	-- "F158"), but each gets its own "Name" naming what it actually
	-- does in this design (e.g. "MEMDMX", "RSNAN1D"). et_device_
	-- purpose ("what the device is doing", visible in the schematic)
	-- is the only native slot for a visible per-instance label, so
	-- that's where this lands. to_purpose raises on length/character
	-- violations regardless of its error_on_invalid_character flag
	-- (that flag is a no-op in the current implementation) -- the
	-- real corpus never exceeds 8 of its 50 allowed characters, but
	-- fall back to empty_purpose rather than aborting the whole
	-- import over one future out-of-range label:
	function extract_purpose (
		props : in pac_properties.map)
		return et_device_purpose.type_device_purpose
	is
		use pac_properties;
		c : constant pac_properties.cursor := find (props, to_property_name ("Name"));
	begin
		if c /= pac_properties.no_element then
			return et_device_purpose.to_purpose (to_string (element (c)));
		else
			return et_device_purpose.empty_purpose;
		end if;
	exception
		when others =>
			return et_device_purpose.empty_purpose;
	end extract_purpose;


	-- Reads one property's absolute "(at x y rot)" and re-expresses
	-- it as a position relative to the placed symbol's own origin, in
	-- ET's Y-up sheet frame. Unlike a pin (defined once, in the
	-- lib_symbol, in a canonical local frame), a KiCad property's
	-- "at" is an ABSOLUTE page coordinate specific to this placement
	-- -- same frame sym.position itself is given in -- so this is a
	-- plain delta against sym.position. Flipping Y for a delta
	-- between two points on the same sheet is just negating the
	-- delta's y component (flip_sheet_y (a, h) - flip_sheet_y (b, h)
	-- = (a.x - b.x, b.y - a.y)); no separate un-rotate/un-mirror step
	-- is needed, since the raw KiCad delta already reflects however
	-- the symbol itself is placed on the page -- exactly the frame
	-- et_device_placeholders.symbols expects a placeholder position
	-- stored in (see rotate_placeholders/draw_placeholders in
	-- et_canvas_schematic-draw_units.adb: a placeholder's position is
	-- baked in at the current placement rotation, not re-rotated at
	-- draw time):
	function placeholder_position (
		sym				: in type_placed_symbol;
		property_name	: in string)
		return et_schematic_geometry.pac_geometry_2.type_vector_model
	is
		use pac_property_placements;
		c : constant pac_property_placements.cursor :=
			find (sym.placements, to_property_name (property_name));
	begin
		if c = pac_property_placements.no_element then
			return (x => 0.0, y => 0.0);
		end if;

		declare
			p : constant type_property_placement := element (c);
		begin
			return (
				x => p.position.x - sym.position.x,
				y => sym.position.y - p.position.y);
		end;
	end placeholder_position;


	-- Maps KiCad's own justify vocabulary onto et_alignment's --
	-- both describe which edge/corner of the text sits AT the anchor
	-- point (e.g. "justify left" = anchor at the text's left edge,
	-- text extends rightward), so this is a direct one-for-one
	-- mapping, no axis flip: unlike a raw y-coordinate, "top"/
	-- "bottom" here is already a visual (on-page) concept in both
	-- tools, and the position itself is what carries the Y-flip (see
	-- placeholder_position/flip_sheet_y) -- reapplying a flip here
	-- would flip it twice:
	function to_alignment (
		h : in type_justify_horizontal;
		v : in type_justify_vertical)
		return et_alignment.type_text_alignment
	is
		horizontal : constant et_alignment.type_text_alignment_horizontal :=
			(case h is
				when JUSTIFY_H_LEFT   => et_alignment.ALIGN_LEFT,
				when JUSTIFY_H_CENTER => et_alignment.ALIGN_CENTER,
				when JUSTIFY_H_RIGHT  => et_alignment.ALIGN_RIGHT);

		vertical : constant et_alignment.type_text_alignment_vertical :=
			(case v is
				when JUSTIFY_V_TOP    => et_alignment.ALIGN_TOP,
				when JUSTIFY_V_CENTER => et_alignment.ALIGN_CENTER,
				when JUSTIFY_V_BOTTOM => et_alignment.ALIGN_BOTTOM);
	begin
		return (horizontal => horizontal, vertical => vertical);
	end to_alignment;


	-- Same lookup as placeholder_position, but for the property's
	-- justify instead of its offset -- falls back to ET's own default
	-- (left/bottom) when the property has no placement data, matching
	-- what an all-defaults placeholder used to render as before this
	-- was tracked at all:
	function placeholder_alignment (
		sym				: in type_placed_symbol;
		property_name	: in string)
		return et_alignment.type_text_alignment
	is
		use pac_property_placements;
		c : constant pac_property_placements.cursor :=
			find (sym.placements, to_property_name (property_name));
	begin
		if c = pac_property_placements.no_element then
			return et_alignment.text_alignment_default;
		end if;

		return to_alignment (element (c).justify_h, element (c).justify_v);
	end placeholder_alignment;


	-- KiCad's Y axis grows downward from the sheet's top-left corner;
	-- ET's grows upward from the bottom-left (see
	-- et_kicad_to_native.transpose's "move" for the exact precedent
	-- this mirrors -- same formula, same reasoning, just derived from
	-- this sheet's own already-parsed paper_height instead of a
	-- separate frame/paper-size lookup). Flips a point that is placed
	-- directly on a sheet (device/unit positions, wire/net
	-- coordinates) -- rotation angles are never adjusted, matching
	-- et_kicad_to_native's own "move" (only the position moves; a
	-- symbol's rotation is meaningful independent of which way Y
	-- grows):
	function flip_sheet_y (
		point			: in et_schematic_geometry.pac_geometry_2.type_vector_model;
		sheet_height	: in type_distance_model)
		return et_schematic_geometry.pac_geometry_2.type_vector_model
	is
	begin
		return (x => point.x, y => sheet_height - point.y);
	end flip_sheet_y;


	------------------------------------------------------------------
	-- SYMBOL BODY GRAPHICS (lines/rectangles/circles/arcs)
	--
	-- The package spec calls this a loader, not a renderer, and that
	-- was true through most of this project's history -- symbol body
	-- graphics were kept as opaque parsed sub-trees and never turned
	-- into anything ET could draw, which is why no symbol outlines
	-- ever appeared on import. This section closes that gap: each
	-- graphic_item.raw sub-tree is walked (using the same et_kicad_v6.
	-- sexp helpers the parser itself uses) and turned into native
	-- et_symbol_shapes geometry. Like pin positions, coordinates are
	-- local to the sub-unit's own origin and are copied as-is, no Y
	-- flip -- confirmed correct for pins, and graphics live in the
	-- exact same local frame.
	------------------------------------------------------------------

	function to_point (x, y : in long_float) return et_schematic_geometry.pac_geometry_2.type_vector_model is
	begin
		return (
			x => et_schematic_geometry.pac_geometry_2.to_distance (type_float_model (x)),
			y => et_schematic_geometry.pac_geometry_2.to_distance (type_float_model (y)));
	end to_point;


	-- Reads a "(tag (at x y ...))"-shaped point child of node, e.g.
	-- (start x y), (end x y), (center x y), (mid x y) or (xy x y):
	function read_point (
		node : in et_kicad_v6.sexp.type_node;
		tag  : in string)
		return et_schematic_geometry.pac_geometry_2.type_vector_model
	is
		n : constant et_kicad_v6.sexp.type_node := et_kicad_v6.sexp.find_first_child (node, tag);
	begin
		return to_point (
			et_kicad_v6.sexp.atom_to_real (et_kicad_v6.sexp.get_child (n, 2)),
			et_kicad_v6.sexp.atom_to_real (et_kicad_v6.sexp.get_child (n, 3)));
	end read_point;


	-- type_line_width is range 0.1 .. 10.0 -- KiCad's own "0 means
	-- use the symbol's default width" convention falls outside that,
	-- same treatment as pin length/device value elsewhere: fall back
	-- to line_width_default rather than raising a range check failure:
	function read_stroke_width (node : in et_kicad_v6.sexp.type_node) return type_line_width is
		stroke_node : constant et_kicad_v6.sexp.type_node :=
			et_kicad_v6.sexp.find_first_child (node, "stroke");
		width_node  : et_kicad_v6.sexp.type_node;
		w			: type_distance_model;
	begin
		if et_kicad_v6.sexp.kind (stroke_node) /= et_kicad_v6.sexp.SEXP_LIST then
			return line_width_default;
		end if;

		width_node := et_kicad_v6.sexp.find_first_child (stroke_node, "width");

		if et_kicad_v6.sexp.kind (width_node) /= et_kicad_v6.sexp.SEXP_LIST then
			return line_width_default;
		end if;

		w := et_schematic_geometry.pac_geometry_2.to_distance (
			type_float_model (et_kicad_v6.sexp.atom_to_real (et_kicad_v6.sexp.get_child (width_node, 2))));

		if w in type_line_width then
			return w;
		else
			return line_width_default;
		end if;
	end read_stroke_width;


	-- The center of the circle through three points, via the standard
	-- circumcenter formula -- done in floating point (fixed-point
	-- arithmetic has no clean way to express the squared terms this
	-- needs) and converted back at the end. KiCad arcs are given as
	-- three points (start/mid/end) with no explicit center, unlike
	-- v4/v5's center+radius+angles form that et_kicad_to_native.
	-- copy_arc could just type-convert directly:
	function arc_center (
		p1, p2, p3 : in et_schematic_geometry.pac_geometry_2.type_vector_model)
		return et_schematic_geometry.pac_geometry_2.type_vector_model
	is
		x1 : constant type_float_model := type_float_model (p1.x);
		y1 : constant type_float_model := type_float_model (p1.y);
		x2 : constant type_float_model := type_float_model (p2.x);
		y2 : constant type_float_model := type_float_model (p2.y);
		x3 : constant type_float_model := type_float_model (p3.x);
		y3 : constant type_float_model := type_float_model (p3.y);

		d : constant type_float_model := 2.0 * (x1 * (y2 - y3) + x2 * (y3 - y1) + x3 * (y1 - y2));
	begin
		if abs d < 1.0e-6 then
			-- degenerate (collinear points) -- not expected from a
			-- real KiCad arc; midpoint of start/end is a defensive
			-- fallback, not a correct circle center:
			return (x => (p1.x + p3.x) / 2.0, y => (p1.y + p3.y) / 2.0);
		end if;

		declare
			ux : constant type_float_model :=
				((x1**2 + y1**2) * (y2 - y3) + (x2**2 + y2**2) * (y3 - y1) + (x3**2 + y3**2) * (y1 - y2)) / d;

			uy : constant type_float_model :=
				((x1**2 + y1**2) * (x3 - x2) + (x2**2 + y2**2) * (x1 - x3) + (x3**2 + y3**2) * (x2 - x1)) / d;
		begin
			return (
				x => et_schematic_geometry.pac_geometry_2.to_distance (ux),
				y => et_schematic_geometry.pac_geometry_2.to_distance (uy));
		end;
	end arc_center;


	-- CW/CCW from the signed area of start/mid/end, in the same
	-- local (unflipped) frame the points themselves are already in:
	function arc_direction (
		start_p, mid_p, end_p : in et_schematic_geometry.pac_geometry_2.type_vector_model)
		return type_direction_of_rotation
	is
		sx : constant type_float_model := type_float_model (start_p.x);
		sy : constant type_float_model := type_float_model (start_p.y);
		mx : constant type_float_model := type_float_model (mid_p.x);
		my : constant type_float_model := type_float_model (mid_p.y);
		ex : constant type_float_model := type_float_model (end_p.x);
		ey : constant type_float_model := type_float_model (end_p.y);

		cross : constant type_float_model := (mx - sx) * (ey - sy) - (my - sy) * (ex - sx);
	begin
		if cross >= 0.0 then
			return CCW;
		else
			return CW;
		end if;
	end arc_direction;


	-- Ada.Containers.Doubly_Linked_Lists has no "&" concatenation
	-- (unlike Vectors) -- used to combine a sub-unit's own pins/
	-- graphics with unit 0's common ones, see build_device_models:
	function combine_pins (a, b : in pac_pins.list) return pac_pins.list is
		result : pac_pins.list := a;
	begin
		for p of b loop
			pac_pins.append (result, p);
		end loop;

		return result;
	end combine_pins;


	function combine_graphics (
		a, b : in pac_symbol_graphics.list)
		return pac_symbol_graphics.list
	is
		result : pac_symbol_graphics.list := a;
	begin
		for g of b loop
			pac_symbol_graphics.append (result, g);
		end loop;

		return result;
	end combine_graphics;


	-- Converts one sub-unit's opaque parsed graphics into native
	-- et_symbol_shapes -- text (GFX_TEXT) and anything unrecognized
	-- (GFX_OTHER) stay out of scope, matching the package spec's
	-- already-stated "pins get full fidelity, body graphics don't
	-- need to" position; only the geometry needed to actually draw a
	-- recognizable outline is converted:
	function convert_shapes (graphics : in pac_symbol_graphics.list) return type_shapes is
		result : type_shapes;
	begin
		for item of graphics loop
			if item.raw /= null then
				declare
					n : et_kicad_v6.sexp.type_node renames item.raw.all;
					w : constant type_line_width := read_stroke_width (n);
				begin
					case item.item_kind is
						when GFX_POLYLINE =>
							declare
								pts_node : constant et_kicad_v6.sexp.type_node :=
									et_kicad_v6.sexp.find_first_child (n, "pts");
								xy_nodes : constant et_kicad_v6.sexp.pac_node_list.vector :=
									et_kicad_v6.sexp.find_all_children (pts_node, "xy");
							begin
								for i in xy_nodes.first_index .. xy_nodes.last_index - 1 loop
									pac_symbol_lines.append (result.lines, (
										pac_geometry_2.type_line (pac_geometry_2.to_line (
											A => to_point (
												et_kicad_v6.sexp.atom_to_real (
													et_kicad_v6.sexp.get_child (xy_nodes (i), 2)),
												et_kicad_v6.sexp.atom_to_real (
													et_kicad_v6.sexp.get_child (xy_nodes (i), 3))),
											B => to_point (
												et_kicad_v6.sexp.atom_to_real (
													et_kicad_v6.sexp.get_child (xy_nodes (i + 1), 2)),
												et_kicad_v6.sexp.atom_to_real (
													et_kicad_v6.sexp.get_child (xy_nodes (i + 1), 3)))))
										with width => w));
								end loop;
							end;

						when GFX_RECTANGLE =>
							declare
								p1 : constant et_schematic_geometry.pac_geometry_2.type_vector_model :=
									read_point (n, "start");
								p2 : constant et_schematic_geometry.pac_geometry_2.type_vector_model :=
									read_point (n, "end");

								corner_2 : constant et_schematic_geometry.pac_geometry_2.type_vector_model :=
									(x => p2.x, y => p1.y);
								corner_4 : constant et_schematic_geometry.pac_geometry_2.type_vector_model :=
									(x => p1.x, y => p2.y);
							begin
								pac_symbol_lines.append (result.lines,
									(pac_geometry_2.type_line (pac_geometry_2.to_line (p1, corner_2)) with width => w));
								pac_symbol_lines.append (result.lines,
									(pac_geometry_2.type_line (pac_geometry_2.to_line (corner_2, p2)) with width => w));
								pac_symbol_lines.append (result.lines,
									(pac_geometry_2.type_line (pac_geometry_2.to_line (p2, corner_4)) with width => w));
								pac_symbol_lines.append (result.lines,
									(pac_geometry_2.type_line (pac_geometry_2.to_line (corner_4, p1)) with width => w));
							end;

						when GFX_CIRCLE =>
							declare
								radius_node : constant et_kicad_v6.sexp.type_node :=
									et_kicad_v6.sexp.find_first_child (n, "radius");

								center : constant et_schematic_geometry.pac_geometry_2.type_vector_model :=
									read_point (n, "center");
								radius : constant type_float_model := type_float_model (
									et_kicad_v6.sexp.atom_to_real (et_kicad_v6.sexp.get_child (radius_node, 2)));
							begin
								pac_symbol_circles.append (result.circles, (
									type_circle_base'(
										pac_geometry_2.type_circle (pac_geometry_2.to_circle (
											center	=> center,
											radius	=> et_schematic_geometry.pac_geometry_2.to_distance (radius)))
										with width => w)
									with filled => NO));
							end;

						when GFX_ARC =>
							declare
								start_p : constant et_schematic_geometry.pac_geometry_2.type_vector_model :=
									read_point (n, "start");
								mid_p   : constant et_schematic_geometry.pac_geometry_2.type_vector_model :=
									read_point (n, "mid");
								end_p   : constant et_schematic_geometry.pac_geometry_2.type_vector_model :=
									read_point (n, "end");
							begin
								pac_symbol_arcs.append (result.arcs, (
									pac_geometry_2.type_arc (pac_geometry_2.to_arc (
										center		=> arc_center (start_p, mid_p, end_p),
										A			=> start_p,
										B			=> end_p,
										direction	=> arc_direction (start_p, mid_p, end_p)))
									with width => w));
							end;

						when GFX_TEXT | GFX_OTHER =>
							null;
					end case;
				end;
			end if;
		end loop;

		return result;
	end convert_shapes;



	-- Makes free text safe to round-trip through et_module_write/
	-- et_module_read's *.mod line format, which has no room for
	-- either of these two things a KiCad text note can freely
	-- contain:
	--
	-- 1. An embedded LF -- KiCad multi-line notes use a literal "\n"
	--    two-character escape that the parser already decodes into a
	--    real LF, but et_module_write writes text content as a single
	--    quoted line, so an embedded LF breaks the file's own line-
	--    oriented format on read-back (the closing quote ends up on
	--    the next physical line, raised as "Missing delimiter").
	--    Collapsed to a single space.
	--
	-- 2. A "--" substring -- et_string_processing.read_line strips
	--    everything from "--" onward as a comment, even inside a
	--    quoted field (comment_mark_default is "--", and the search
	--    for it is not quote-aware). A hand-written divider line like
	--    "------" silently eats its own closing quote the same way as
	--    (1), confirmed with a standalone reproduction against
	--    read_line directly. Every run of 2+ dashes gets a space
	--    inserted between each pair, so no "--" substring survives
	--    (a divider still reads as one, just spaced out):
	function sanitize_text_content (raw : in string) return string is
		use ada.strings.unbounded;
		buf			: unbounded_string;
		prev_dash	: boolean := false;
	begin
		for ch of raw loop
			if ch = ASCII.LF or ch = ASCII.CR then
				append (buf, ' ');
				prev_dash := false;

			elsif ch = '-' then
				if prev_dash then
					append (buf, ' ');
				end if;

				append (buf, ch);
				prev_dash := true;

			else
				append (buf, ch);
				prev_dash := false;
			end if;
		end loop;

		return to_string (buf);
	end sanitize_text_content;


	-- Parses a KiCad title_block date, "DD-MON-YY" (e.g. "22-MAY-90"),
	-- into a native time value via et_time.to_date, which itself
	-- expects ISO "YYYY-MM-DD". The two-digit year is expanded using
	-- the common 69/00 pivot (00 .. 68 -> 2000 .. 2068, 69 .. 99 ->
	-- 1969 .. 1999) -- appropriate here since every date actually
	-- seen in this project is firmly in the 1900s (a decades-old
	-- design being hand-digitized). Raises constraint_error on
	-- anything not matching this exact shape; the caller falls back
	-- to a default and logs a warning:
	function parse_kicad_date (raw : in string) return time is
		use ada.characters.handling;

		months : constant array (1 .. 12) of string (1 .. 3) :=
			("JAN", "FEB", "MAR", "APR", "MAY", "JUN",
			 "JUL", "AUG", "SEP", "OCT", "NOV", "DEC");

		day_str		: constant string := raw (raw'first .. raw'first + 1);
		mon_str		: constant string := to_upper (raw (raw'first + 3 .. raw'first + 5));
		year_2d		: constant natural := natural'value (raw (raw'first + 7 .. raw'first + 8));
		year_4d		: constant natural := (if year_2d <= 68 then 2000 + year_2d else 1900 + year_2d);

		month_num	: natural := 0;
	begin
		if raw'length /= 9 or raw (raw'first + 2) /= '-' or raw (raw'first + 6) /= '-' then
			raise constraint_error;
		end if;

		for m in months'range loop
			if months (m) = mon_str then
				month_num := m;
			end if;
		end loop;

		if month_num = 0 then
			raise constraint_error;
		end if;

		return et_time.to_date (
			trim (natural'image (year_4d), left) & "-"
			& (if month_num < 10 then "0" else "") & trim (natural'image (month_num), left) & "-"
			& day_str);
	end parse_kicad_date;


	-- et_port_general.type_port_general's rotation field is
	-- type_rotation_relative, range -90.0 .. 180.0 -- much narrower
	-- than a KiCad pin's 0/90/180/270 orientation (270 alone is
	-- already out of range). Normalizes by +/-360.0 steps into that
	-- range (270 -> -90, etc.):
	function normalize_rotation (
		r : in type_rotation_model)
		return type_rotation_relative
	is
		result : type_rotation_model := r;
	begin
		while result > rotation_relative_max loop
			result := result - 360.0;
		end loop;

		while result < rotation_relative_min loop
			result := result + 360.0;
		end loop;

		return result;
	end normalize_rotation;


	-- Maps one KiCad pin onto one native symbol port. The electrical-
	-- type mapping is necessarily lossy in several cases: KiCad
	-- distinguishes tri_state/open_collector/open_emitter, none of
	-- which have an exact et_port_direction.type_port_direction
	-- counterpart, and KiCad's pin type alone carries no
	-- digital/analog distinction at all (unlike et_port_direction,
	-- which requires picking one). Fields with no KiCad source data
	-- (tristate/weakness/inverted/edge/level/power level) are filled
	-- with each type's own "_default" constant rather than invented
	-- values:
	function build_port (
		pin : in type_pin)
		return type_symbol_port
	is
		-- Pin positions are local to the sub-unit's own origin, not a
		-- sheet -- copied as-is, no Y flip (unlike sheet-level
		-- placement/wire coordinates, see flip_sheet_y). type_port_
		-- length is range 2.0 .. 20.0 -- KiCad pin lengths outside
		-- that range (0.0 for a hidden power pin, or anything else out
		-- of bounds) fall back to the default rather than raising a
		-- range check failure.
		--
		-- et_symbol_port_general's own doc comment gives the tail
		-- (into-body) direction per rotation: 0=left, 90=down,
		-- 180=right, 270=up -- i.e. standard_angle = rotation + 180,
		-- and this direction is applied to the port's position
		-- (unflipped, raw KiCad numbers, as above) exactly like KiCad
		-- applies its own pin angle to that same raw position:
		-- tail = position + length * (cos(angle), sin(angle)). For
		-- both tools to compute the *same* raw tail point (so the
		-- stub still visually reaches wherever the symbol's own body
		-- geometry actually is, even though that body is never drawn),
		-- rotation + 180 must equal pin.orientation, i.e.
		-- rotation = pin.orientation - 180.
		--
		-- (A first attempt used 180.0 - pin.orientation instead --
		-- reasoning by ad hoc "which way is visually up/down" instead
		-- of solving this equation. That only coincidentally agrees
		-- with the correct formula at pin.orientation = 0 and 180 (the
		-- values self-mirror under negation), which is exactly why
		-- r1000:8051's mostly-0/180 pins looked right while r1000:PU/
		-- PD -- both 90/270, a single power pin each -- came out
		-- exactly 180 degrees backwards.)
		--
		-- Re-verified against the same three r1000:8051 pins as
		-- before, this time solving for where the tail must land
		-- (matching the symbol's own rectangle body edges) rather than
		-- checking the formula against its own prior output: KiCad 0
		-- -> rotation 180, KiCad 180 -> rotation 0 (both unchanged from
		-- the first attempt), KiCad 270 -> rotation 90 (was wrongly
		-- -90). All three still land inside type_rotation_relative's
		-- -90.0 .. 180.0 range with no wraparound needed:
		base : constant type_port_general := (
			position	=> pin.position,
			length		=> (if pin.length in type_port_length then pin.length else port_length_default),
			rotation	=> normalize_rotation (pin.orientation - 180.0));

		root : constant type_port_base := (base with others => <>);
	begin
		case pin.electrical_type is

			when PIN_PASSIVE | PIN_FREE | PIN_UNSPECIFIED =>
				return (root with direction => PASSIVE);

			when PIN_NO_CONNECT =>
				return (root with direction => NOT_CONNECTED);

			when PIN_INPUT =>
				return (root with direction => INPUT_ANALOG);

			when PIN_OUTPUT =>
				return (root with
					direction				=> OUTPUT_ANALOG,
					output_analog_tristate	=> output_tristate_default,
					output_analog_weakness	=> output_weakness_default);

			when PIN_BIDIRECTIONAL =>
				return (root with
					direction				=> BIDIR_DIGITAL,
					output_inverted			=> output_inverted_default,
					output_tristate			=> output_tristate_default,
					output_weakness			=> output_weakness_default,
					input_sensitivity_edge		=> sensitivity_edge_default,
					input_sensitivity_level	=> sensitivity_level_default);

			-- CS: tri_state/open_collector/open_emitter all map onto
			-- OUTPUT_DIGITAL with tristate forced YES -- the closest
			-- available approximation, not an exact equivalent:
			when PIN_TRI_STATE | PIN_OPEN_COLLECTOR | PIN_OPEN_EMITTER =>
				return (root with
					direction				=> OUTPUT_DIGITAL,
					output_digital_inverted	=> output_inverted_default,
					output_digital_tristate	=> YES,
					output_digital_weakness	=> output_weakness_default);

			when PIN_POWER_IN =>
				return (root with direction => POWER_IN, level => port_power_level_default);

			when PIN_POWER_OUT =>
				return (root with direction => POWER_OUT, level => port_power_level_default);

		end case;
	end build_port;


	-- Builds a native ports map from a sub-unit's pin list. Duplicate
	-- pin names (the same unit having e.g. two "GND" pins) cannot be
	-- represented -- pac_symbol_ports is keyed by name -- so the first
	-- occurrence wins and every later duplicate is logged and skipped:
	procedure build_ports (
		pins			: in pac_pins.list;
		lib_id			: in type_lib_id;
		log_threshold	: in type_log_level;
		ports			: in out pac_symbol_ports.map)
	is
		inserted	: boolean;
		port_cursor	: pac_symbol_ports.cursor;
	begin
		for p of pins loop
			declare
				-- KiCad allows an explicitly empty pin name (common
				-- for power symbols with pin_names hidden, e.g.
				-- "(name "" ...)") -- et_symbol_write_ports always
				-- writes "name <text>" with a mandatory argument, so
				-- an empty name here would produce an unparseable
				-- *.dev line. Fall back to the pin number, which
				-- KiCad guarantees is present:
				raw_name : constant string := to_string (p.name);

				name : constant et_port_names.type_port_name :=
					et_port_names.to_port_name (
						(if raw_name'length > 0 then raw_name else to_string (p.number)));
			begin
				if pac_symbol_ports.contains (ports, name) then
					log (SEVERITY_NOTE,
						text	=> "duplicate pin name '" & to_string (p.name) & "' on symbol "
							& to_string (lib_id) & " -> keeping first occurrence",
						level	=> log_threshold + 3);
				else
					pac_symbol_ports.insert (
						container	=> ports,
						key			=> name,
						position	=> port_cursor,
						inserted	=> inserted,
						new_item	=> build_port (p));
				end if;
			end;
		end loop;
	end build_ports;


	-- Parses a sheet node's raw page number (as filled in by
	-- import_design from the root's sheet_instances block). Sheet
	-- numbers map directly onto native sheet numbers -- no shifting --
	-- so page "3" is native sheet 3. Returns 0 if the page is
	-- unresolved or not a plain integer (KiCad also allows fully
	-- custom/alphanumeric page labels, which this does not attempt to
	-- parse); page 0 -- whether a genuine KiCad page "0"/"00" or one
	-- of these fallback cases -- has no native sheet (et_sheets.
	-- type_sheet requires a minimum of 1) and its content is skipped
	-- by walk, not remapped onto some other sheet:
	function raw_page_number (
		node			: in type_sheet_node_access;
		log_threshold	: in type_log_level)
		return natural
	is
		text : constant string := to_string (node.page);
	begin
		if text'length = 0 then
			return 0;
		end if;

		return natural'value (text);

	exception
		when others =>
			log (SEVERITY_WARNING,
				text	=> "sheet page number '" & text & "' not a plain integer -> treated as page 0 (ignored)",
				level	=> log_threshold);
			return 0;
	end raw_page_number;


	-- Synthesizes one ET device model per distinct lib_id found in the
	-- project's project-wide symbol fold, and inserts it into the
	-- rig-wide device_library -- see et_device_library.get_device_model:
	-- a valid model_cursor can only be assigned to a device if the
	-- model has already been inserted there beforehand, exactly as
	-- et_kicad_to_native.query_components does for v4/v5 imports.
	procedure build_device_models (
		symbols			: in pac_lib_symbols.map;
		log_threshold	: in type_log_level)
	is
		package pac_unit_numbers is new ada.containers.ordered_sets (natural);

		procedure build_one (c : in pac_lib_symbols.cursor) is
			sym			: constant type_lib_symbol := pac_lib_symbols.element (c);
			model_name	: constant type_device_model_name := to_file_name (
				compose (compose (et_project.directory_libraries, et_project.directory_libraries_devices),
					library_of (sym.lib_id) & "_" & symbol_of (sym.lib_id)));

			appearance		: constant type_appearance :=
				(if sym.is_power then APPEARANCE_VIRTUAL else APPEARANCE_PCB);

			units_internal	: pac_units_internal.map;
			unit_numbers	: pac_unit_numbers.set;

			model_cursor	: pac_device_models.cursor;
			inserted		: boolean;
		begin
			if contains (device_library, model_name) then
				return; -- already synthesized -- symbols is already deduped by lib_id
			end if;

			log (text => "device model " & to_string (model_name) & " ...", level => log_threshold + 1);
			log_indentation_up;

			for su of sym.sub_units loop
				if su.key.unit > 0 then
					pac_unit_numbers.include (unit_numbers, su.key.unit);
				end if;
			end loop;

			if pac_unit_numbers.is_empty (unit_numbers) then
				pac_unit_numbers.include (unit_numbers, 1);
			end if;

			-- Unit 0 ("common to all units") frequently carries the
			-- symbol's own body outline separately from unit N's own
			-- pins -- e.g. r1000:F02 has both F02_0_1 (the arc/
			-- polylines making up the gate body) and F02_1_1 (just the
			-- pins). get_sub_unit (sym, u, 1) alone only ever returns
			-- unit u's own entry, silently dropping unit 0's graphics
			-- (and pins, if any project ever puts pins there too)
			-- whenever u itself is non-zero -- which unit_numbers
			-- guarantees it always is, so this is not a redundant
			-- double-fetch of the same data:
			declare
				common : constant type_symbol_sub_unit := get_sub_unit (sym, 0, 1);
			begin
				for u of unit_numbers loop
					declare
						sub			: constant type_symbol_sub_unit := get_sub_unit (sym, u, 1);
						pins		: constant pac_pins.list := combine_pins (common.pins, sub.pins);
						graphics	: constant pac_symbol_graphics.list :=
							combine_graphics (common.graphics, sub.graphics);
						ports		: pac_symbol_ports.map;
						unit_name	: constant et_unit_name.type_unit_name :=
							et_unit_name.to_unit_name (trim (natural'image (u), left));

						unit_cursor		: pac_units_internal.cursor;
						unit_inserted	: boolean;
					begin
						build_ports (pins, sym.lib_id, log_threshold, ports);

						-- NOTE: type_symbol_model has a variant part governed by
						-- "appearance" -- an aggregate using "others => <>" for
						-- it requires a STATIC discriminant (Ada needs to know
						-- at compile time which variant "others" fills in), so
						-- this must branch on a literal, not the runtime
						-- "appearance" value, in each arm:
						case appearance is
							when APPEARANCE_VIRTUAL =>
								declare
									symbol_model : constant type_symbol_model (APPEARANCE_VIRTUAL) := (
										type_symbol_base'(texts => et_symbol_text.pac_symbol_texts.empty_list)
										with
											appearance	=> APPEARANCE_VIRTUAL,
											shapes		=> convert_shapes (graphics),
											ports		=> ports);

									u_internal : constant type_unit_internal (APPEARANCE_VIRTUAL) := (
										appearance	=> APPEARANCE_VIRTUAL,
										symbol		=> symbol_model,
										position	=> (0.0, 0.0),
										others		=> <>);
								begin
									pac_units_internal.insert (
										container	=> units_internal,
										key			=> unit_name,
										position	=> unit_cursor,
										inserted	=> unit_inserted,
										new_item	=> u_internal);
								end;

							when APPEARANCE_PCB =>
								declare
									symbol_model : constant type_symbol_model (APPEARANCE_PCB) := (
										type_symbol_base'(texts => et_symbol_text.pac_symbol_texts.empty_list)
										with
											appearance		=> APPEARANCE_PCB,
											shapes			=> convert_shapes (graphics),
											ports			=> ports,
											placeholders	=> (others => <>));

									u_internal : constant type_unit_internal (APPEARANCE_PCB) := (
										appearance	=> APPEARANCE_PCB,
										symbol		=> symbol_model,
										position	=> (0.0, 0.0),
										others		=> <>);
								begin
									pac_units_internal.insert (
										container	=> units_internal,
										key			=> unit_name,
										position	=> unit_cursor,
										inserted	=> unit_inserted,
										new_item	=> u_internal);
								end;
						end case;
					end;
				end loop;
			end;

			case appearance is
				when APPEARANCE_VIRTUAL =>
					pac_device_models.insert (
						container	=> device_library,
						key			=> model_name,
						position	=> model_cursor,
						inserted	=> inserted,
						new_item	=> (
							appearance		=> APPEARANCE_VIRTUAL,
							prefix			=> extract_prefix (sym.properties),
							units_internal	=> units_internal,
							units_external	=> pac_units_external.empty_map));

				when APPEARANCE_PCB =>
					pac_device_models.insert (
						container	=> device_library,
						key			=> model_name,
						position	=> model_cursor,
						inserted	=> inserted,
						new_item	=> (
							appearance		=> APPEARANCE_PCB,
							prefix			=> extract_prefix (sym.properties),
							units_internal	=> units_internal,
							units_external	=> pac_units_external.empty_map,
							value			=> extract_value (sym.properties),
							variants		=> placeholder_variants));
			end case;

			log_indentation_down;
		end build_one;

	begin
		log (text => "synthesizing device models ...", level => log_threshold);
		log_indentation_up;

		pac_lib_symbols.iterate (symbols, build_one'access);

		log_indentation_down;
	end build_device_models;


	------------------------------------------------------------------
	-- CONVERT
	------------------------------------------------------------------

	function convert (
		project			: in et_kicad_v6.schematic.type_project;
		log_threshold	: in type_log_level)
		return type_generic_module
	is
		module			: type_generic_module;
		anonymous_index	: et_net_names.type_anonymous_net_index := 1;

		-- et_module.type_generic_module.frames.descriptions drives the
		-- GUI's sheet count/navigation (see
		-- et_drawing_frame.schematic.get_sheet_count: an empty
		-- descriptions vector is quietly treated as "1 sheet", which is
		-- why an unpopulated module only ever shows a single sheet
		-- regardless of how many sheets its devices/nets reference).
		-- Collected per sheet number during the tree walk, then turned
		-- into that vector once the highest sheet number is known:
		package pac_sheet_titles is new ada.containers.ordered_maps
			(type_sheet, ada.strings.unbounded.unbounded_string,
				"=" => ada.strings.unbounded."=");
		use pac_sheet_titles;
		sheet_titles : pac_sheet_titles.map;

		-- module.meta and module.frames.frame's paper size are both
		-- project-wide (not per-sheet, unlike sheet_titles above), so
		-- these are captured once from the first sheet that actually
		-- has the data -- confirmed uniform across every sampled
		-- sheet in the real SEQ project, so "first wins" isn't a
		-- meaningful loss of information there. See build_meta_and_
		-- frame, called from walk:
		meta_captured : boolean := false;


		-- Builds (or extends, for a device already seen on another
		-- sheet -- multi-unit devices are placed as several separate
		-- placed_symbol instances sharing one reference) one native
		-- device from one placed symbol occurrence:
		procedure build_device (
			sym			: in type_placed_symbol;
			containing	: in type_sheet_node_access;
			sheet_num	: in type_sheet)
		is
			inst	: type_instance_ref;
			found	: boolean := false;

			model_name		: type_device_model_name;
			model_cursor	: pac_device_models.cursor;
			device_name		: et_device_name.type_device_name;
			device_cursor	: pac_devices_electrical.cursor;
			device_inserted	: boolean;
			unit_name		: et_unit_name.type_unit_name;


			procedure add_unit (
				key		: in et_device_name.type_device_name;
				element	: in out type_device_electrical)
			is
				pragma unreferenced (key);
				unit_inserted	: boolean;
				unit_cursor		: pac_units.cursor;

				unit_position : constant type_object_position := to_position (
					point		=> flip_sheet_y (sym.position, containing.data.paper_height),
					sheet		=> sheet_num,
					rotation	=> sym.orientation);

				new_unit : type_unit (element.appearance);
			begin
				case element.appearance is
					when APPEARANCE_VIRTUAL =>
						new_unit := (
							appearance		=> APPEARANCE_VIRTUAL,
							position		=> unit_position,
							mirror_status	=> sym.mirror,
							status			=> object_status_default);

					when APPEARANCE_PCB =>
						declare
							-- KiCad's "Reference"/"Value"/"Name" property
							-- positions (see placeholder_position) map onto
							-- ET's NAME/VALUE/PURPOSE placeholder meanings
							-- respectively -- "Name" here is this project's
							-- own per-instance functional-label convention,
							-- same as extract_purpose above:
							plc : type_text_placeholders := (others => <>);
						begin
							plc.name.position    := placeholder_position (sym, "Reference");
							plc.value.position   := placeholder_position (sym, "Value");
							plc.purpose.position := placeholder_position (sym, "Name");

							plc.name.alignment    := placeholder_alignment (sym, "Reference");
							plc.value.alignment   := placeholder_alignment (sym, "Value");
							plc.purpose.alignment := placeholder_alignment (sym, "Name");

							new_unit := (
								appearance		=> APPEARANCE_PCB,
								position		=> unit_position,
								mirror_status	=> sym.mirror,
								status			=> object_status_default,
								placeholders	=> plc);
						end;
				end case;

				pac_units.insert (
					container	=> element.units,
					key			=> unit_name,
					position	=> unit_cursor,
					inserted	=> unit_inserted,
					new_item	=> new_unit);
			end add_unit;

		begin
			for ir of sym.instances loop
				if ir.path = containing.uuid_path then
					inst := ir;
					found := true;
					exit;
				end if;
			end loop;

			if not found and then not sym.instances.is_empty then
				inst := sym.instances.first_element;
				found := true;
			end if;

			if not found then
				log (SEVERITY_WARNING,
					text	=> "placed symbol " & to_string (sym.lib_id) & " has no instance data -> skipped",
					level	=> log_threshold);
				return;
			end if;

			model_name := to_file_name (
				compose (compose (et_project.directory_libraries, et_project.directory_libraries_devices),
					library_of (sym.lib_id) & "_" & symbol_of (sym.lib_id)));

			model_cursor := get_device_model (model_name);

			if model_cursor = pac_device_models.no_element then
				log (SEVERITY_WARNING,
					text	=> "no device model synthesized for " & to_string (sym.lib_id) & " -> skipped",
					level	=> log_threshold);
				return;
			end if;

			-- KiCad's virtual/power-symbol references carry a leading
			-- '#' (e.g. "#PWR0104", "#FLG01") that et_device_name
			-- rejects outright -- strip it, mirroring
			-- et_kicad_to_native's remove_leading_hash for the same
			-- purpose:
			declare
				ref_text : constant string := to_string (inst.reference);
			begin
				if ref_text'length > 0 and then ref_text (ref_text'first) = '#' then
					device_name := et_device_name.to_device_name (ref_text (ref_text'first + 1 .. ref_text'last));
				else
					device_name := et_device_name.to_device_name (ref_text);
				end if;
			end;
			unit_name   := et_unit_name.to_unit_name (trim (positive'image (sym.unit), left));

			case element (model_cursor).appearance is
				when APPEARANCE_VIRTUAL =>
					pac_devices_electrical.insert (
						container	=> module.devices,
						key			=> device_name,
						position	=> device_cursor,
						inserted	=> device_inserted,
						new_item	=> (
							appearance		=> APPEARANCE_VIRTUAL,
							model_cursor	=> model_cursor,
							others			=> <>));

				when APPEARANCE_PCB =>
					pac_devices_electrical.insert (
						container	=> module.devices,
						key			=> device_name,
						position	=> device_cursor,
						inserted	=> device_inserted,
						new_item	=> (
							appearance		=> APPEARANCE_PCB,
							model_cursor	=> model_cursor,
							value			=> sanitize_device_value (to_string (inst.value)),
							partcode		=> et_device_partcode.to_partcode (et_device_partcode.partcode_default),
							purpose			=> extract_purpose (sym.properties),
							-- No footprint/package data in this KiCad
							-- schematic-only project (out of scope --
							-- see the package spec) -- et_module_write
							-- always writes "variant <name>" with a
							-- mandatory argument, so an empty name here
							-- would produce an unparseable *.mod line:
							variant			=> et_package_variant_name.to_variant_name (placeholder_not_assigned),
							position		=> et_board_coordinates.package_position_default,
							placeholders	=> (others => <>),
							status			=> object_status_default,
							others			=> <>));
			end case;

			pac_devices_electrical.update_element (
				container	=> module.devices,
				position	=> device_cursor,
				process		=> add_unit'access);
		end build_device;


		-- Contributes one sheet-local strand's wire/bus-derived
		-- segments to the flattened project-wide net map. Bus wires
		-- are intentionally not expanded into per-signal segments --
		-- a bus aggregates multiple nets, and drawing its geometry as
		-- if it belonged to one net would misrepresent it -- so only
		-- plain wires contribute segments (out of scope, not an
		-- oversight):
		procedure build_net_contribution (
			sheet_num	: in type_sheet;
			strand		: in type_strand;
			sheet		: in type_sheet_data
		)
		is
			segments : pac_net_segments.list;

			-- to_net_segment defaults both ends to "no junction" --
			-- KiCad's own (junction ...) markers (type_sheet_data.
			-- junctions, already parsed) are a separate list from the
			-- wires themselves and were never consulted when building
			-- native segments, so no junction dot ever appeared on
			-- import regardless of how many the source actually had.
			-- Raw/unflipped comparison, same reasoning as the strand.
			-- points membership check below:
			function has_junction_at (p : in et_schematic_geometry.pac_geometry_2.type_vector_model) return boolean is
			begin
				for j of sheet.junctions loop
					if et_schematic_geometry.pac_geometry_2."=" (j.position, p) then
						return true;
					end if;
				end loop;

				return false;
			end has_junction_at;

			-- True if p lies on the (raw, unflipped) segment a-b -- an
			-- exact endpoint always counts; otherwise only handles the
			-- axis-aligned (purely horizontal or vertical) case, which
			-- covers virtually every wire in a hand-drawn schematic. A
			-- label sitting on a genuinely diagonal wire, away from
			-- either of its endpoints, won't be found this way:
			function point_on_segment (
				p, a, b : in et_schematic_geometry.pac_geometry_2.type_vector_model)
				return boolean
			is
				use et_schematic_geometry.pac_geometry_2;
			begin
				if p = a or p = b then
					return true;
				end if;

				if a.y = b.y and p.y = a.y then -- horizontal
					return p.x in type_distance_model'min (a.x, b.x) .. type_distance_model'max (a.x, b.x);
				end if;

				if a.x = b.x and p.x = a.x then -- vertical
					return p.y in type_distance_model'min (a.y, b.y) .. type_distance_model'max (a.y, b.y);
				end if;

				return false;
			end point_on_segment;

			-- Attaches a native label to this segment for every one of
			-- the strand's own labels that sits on it -- labels were
			-- parsed onto the strand as a whole (et_kicad_v6.schematic
			-- doesn't track which specific wire each one sits on), so
			-- this reconnects each one to a segment the same way KiCad
			-- itself does: by shared position. The net's own name is
			-- unaffected either way, since that comes from
			-- strand.labels directly, not from these native label
			-- objects:
			procedure append_matching_labels (
				a, b	: in et_schematic_geometry.pac_geometry_2.type_vector_model;
				target	: in out pac_net_labels.list)
			is
			begin
				for lbl of strand.labels loop
					if point_on_segment (lbl.position, a, b) then
						pac_net_labels.append (target, (
							type_net_label_base'(others => <>)
							with
								position => flip_sheet_y (lbl.position, sheet.paper_height),
								rotation =>
									(if lbl.orientation = 90.0 or lbl.orientation = 270.0
									 then VERTICAL else HORIZONTAL)));
					end if;
				end loop;
			end append_matching_labels;

			procedure collect_from_wire (pts : in pac_points.vector) is
			begin
				if pts.length < 2 then
					return;
				end if;

				for i in pts.first_index .. pts.last_index - 1 loop
					-- Membership is tested against the strand's own
					-- (still raw/unflipped) points -- those were
					-- grouped by the parser's union-find over the same
					-- raw KiCad coordinates, so the match must happen
					-- before flip_sheet_y is applied to build the
					-- actual native segment:
					if pac_points.contains (strand.points, pts (i)) then
						declare
							segment : type_net_segment := to_net_segment (
								flip_sheet_y (pts (i), sheet.paper_height),
								flip_sheet_y (pts (i + 1), sheet.paper_height));
						begin
							segment.junctions.A := has_junction_at (pts (i));
							segment.junctions.B := has_junction_at (pts (i + 1));

							append_matching_labels (pts (i), pts (i + 1), segment.labels);

							pac_net_segments.append (segments, segment);
						end;
					end if;
				end loop;
			end collect_from_wire;

			net_name	: et_net_names.type_net_name;
			scope		: type_net_scope;

			global_found, other_found	: boolean := false;
			global_lbl, other_lbl		: type_label;

			net_cursor		: et_nets.pac_nets.cursor;
			net_inserted	: boolean;
		begin
			for w of sheet.wires loop
				collect_from_wire (w.points);
			end loop;

			if segments.is_empty then
				return;
			end if;

			for lbl of strand.labels loop
				if lbl.label_kind = LABEL_GLOBAL then
					if not global_found then
						global_lbl := lbl;
						global_found := true;
					end if;
				else
					if not other_found then
						other_lbl := lbl;
						other_found := true;
					end if;
				end if;
			end loop;

			if global_found then
				net_name	:= et_net_names.to_net_name (to_string (global_lbl.text));
				scope		:= GLOBAL;

			elsif other_found then
				net_name := et_net_names.to_net_name (
					to_string (other_lbl.text) & "_SH" & trim (type_sheet'image (sheet_num), left));
				scope := LOCAL;

				if other_lbl.label_kind = LABEL_HIERARCHICAL then
					log (SEVERITY_NOTE,
						text	=> "hierarchical label " & to_string (other_lbl.text)
							& " treated as sheet-local (hierarchical sheet-pin promotion not implemented)",
						level	=> log_threshold + 3);
				end if;

			else
				net_name := et_net_names.to_anonymous_net_name (anonymous_index);
				anonymous_index := anonymous_index + 1;
				scope := LOCAL;
			end if;

			et_nets.pac_nets.insert (
				container	=> module.nets,
				key			=> net_name,
				position	=> net_cursor,
				inserted	=> net_inserted,
				new_item	=> (scope => scope, others => <>));

			declare
				strand_native : et_net_strands.type_strand;

				procedure append_strand (
					key		: in et_net_names.type_net_name;
					element	: in out et_nets.type_net)
				is
					pragma unreferenced (key);
				begin
					et_net_strands.pac_strands.append (element.strands, strand_native);
				end append_strand;

			begin
				strand_native.segments := segments;

				-- Computes strand_native.position.place from the
				-- segments just assigned (the point closest to the
				-- drawing origin) -- et_module_read_nets re-derives
				-- and cross-checks this on read, flagging anything
				-- else as "Lowest x/y position of strand invalid":
				et_net_strands.set_strand_position (strand_native);
				strand_native.position.sheet := sheet_num;

				et_nets.pac_nets.update_element (
					container	=> module.nets,
					position	=> net_cursor,
					process		=> append_strand'access);
			end;
		end build_net_contribution;


		-- Converts one standalone KiCad (text ...) note (not a net
		-- label -- those are handled separately, see
		-- append_matching_labels) into a native schematic text/note,
		-- appended to module.texts:
		procedure build_free_text (
			txt			: in type_free_text;
			sheet_num	: in type_sheet;
			paper_height	: in type_distance_model)
		is
			raw			: constant string := to_string (txt.text);
			sanitized	: constant string := sanitize_text_content (raw);

			-- type_text_content allows at most text_length_max
			-- characters -- KiCad free text is not expected to be
			-- anywhere near that long, but this must not raise if one
			-- ever is (same reasoning as the sheet description text in
			-- convert's finalization):
			clipped : constant string := sanitized (
				sanitized'first .. sanitized'first - 1 +
					natural'min (sanitized'length, et_text_content.text_length_max));
		begin
			et_schematic_text.pac_texts.append (module.texts, (
				pac_text_schematic.type_text'(
					alignment	=> to_alignment (txt.justify_h, txt.justify_v),
					others		=> <>)
				with
					position	=> flip_sheet_y (txt.position, paper_height),
					rotation	=> (if txt.orientation = 90.0 or txt.orientation = 270.0
									then VERTICAL else HORIZONTAL),
					sheet		=> sheet_num,
					content		=> et_text_content.to_content (clipped)));
		end build_free_text;


		-- Converts a placed symbol's "Location" property -- this
		-- project's own convention for the part's grid reference on
		-- the original paper drawing (e.g. "H16") -- into a
		-- standalone schematic text at its own absolute position.
		-- Unlike "Reference"/"Value"/"Name" (which land on the
		-- device itself as NAME/VALUE/PURPOSE placeholders, see
		-- build_device/placeholder_position), "Location" has no
		-- matching device concept in ET, so it becomes a free-
		-- standing text object instead -- the closest native
		-- equivalent to "just show this text where KiCad had it":
		procedure build_location_text (
			sym			: in type_placed_symbol;
			sheet_num	: in type_sheet;
			paper_height	: in type_distance_model)
		is
			use pac_properties;
			use pac_property_placements;

			prop_c : constant pac_properties.cursor :=
				find (sym.properties, to_property_name ("Location"));
		begin
			if prop_c = pac_properties.no_element then
				return;
			end if;

			declare
				raw			: constant string := to_string (element (prop_c));
				sanitized	: constant string := sanitize_text_content (raw);

				clipped : constant string := sanitized (
					sanitized'first .. sanitized'first - 1 +
						natural'min (sanitized'length, et_text_content.text_length_max));

				place_c : constant pac_property_placements.cursor :=
					find (sym.placements, to_property_name ("Location"));

				-- Absolute page position, same as any other property
				-- -- fall back to the symbol's own position if this
				-- particular file never gave "Location" an "at":
				abs_position : constant et_schematic_geometry.pac_geometry_2.type_vector_model :=
					(if place_c /= pac_property_placements.no_element
					 then element (place_c).position
					 else sym.position);

				orientation : constant type_rotation_model :=
					(if place_c /= pac_property_placements.no_element
					 then element (place_c).rotation
					 else 0.0);

				alignment : constant et_alignment.type_text_alignment :=
					(if place_c /= pac_property_placements.no_element
					 then to_alignment (element (place_c).justify_h, element (place_c).justify_v)
					 else et_alignment.text_alignment_default);
			begin
				if clipped'length = 0 then
					return;
				end if;

				et_schematic_text.pac_texts.append (module.texts, (
					pac_text_schematic.type_text'(
						alignment	=> alignment,
						others		=> <>)
					with
						position	=> flip_sheet_y (abs_position, paper_height),
						rotation	=> (if orientation = 90.0 or orientation = 270.0
										then VERTICAL else HORIZONTAL),
						sheet		=> sheet_num,
						content		=> et_text_content.to_content (clipped)));
			end;
		end build_location_text;


		-- Captures module.meta.schematic (revision/drawing_number/
		-- drawn_date, from title_block) and module.frames.frame's
		-- paper size (from the sheet's own already-parsed paper_
		-- width/height) -- both project-wide, so only the first
		-- sheet with real data (non-empty revision/date) is used; see
		-- meta_captured:
		procedure build_meta_and_frame (sheet : in type_sheet_data) is
			use et_drawing_frame;

			revision_raw : constant string := to_string (sheet.revision);
			date_raw     : constant string := to_string (sheet.date);
			drawing_raw  : constant string := to_string (sheet.comment_2);
		begin
			if meta_captured or revision_raw'length = 0 or date_raw'length = 0 then
				return;
			end if;

			module.meta.schematic.revision := et_meta.to_revision (
				revision_raw (revision_raw'first .. revision_raw'first - 1 +
					natural'min (revision_raw'length, et_meta.revision_length_max)));

			if drawing_raw'length > 0 then
				module.meta.schematic.drawing_number := et_meta.to_drawing_number (
					drawing_raw (drawing_raw'first .. drawing_raw'first - 1 +
						natural'min (drawing_raw'length, et_meta.drawing_number_length_max)));
			end if;

			begin
				module.meta.schematic.drawn_date := parse_kicad_date (date_raw);
			exception
				when others =>
					log (SEVERITY_WARNING,
						text	=> "title_block date '" & date_raw & "' not in the expected "
							& "DD-MON-YY form -> drawn_date left at its default",
						level	=> log_threshold);
			end;

			-- module.frames.frame (paper/orientation/size) is never
			-- actually written by et_module_write_frames -- only
			-- .template (a *.frs template FILE reference) and
			-- .descriptions round-trip through *.mod. Setting .frame
			-- here would be silently inert: it gets recomputed from
			-- whatever .template points to (template_schematic_
			-- default, a placeholder "dummy" file) the next time the
			-- project is opened, not read back from the file this
			-- converter writes. Representing this project's actual
			-- paper size for real would mean generating a custom
			-- *.frs template file (et_drawing_frame.type_paper_size's
			-- A3/A4 tag is just a label -- a template's own "size x .."
			-- line is freely settable) and pointing .template at it --
			-- a new capability, not a conversion fix, so it's out of
			-- scope here. Content position itself (flip_sheet_y)
			-- already uses this sheet's real paper_height regardless
			-- of whatever frame is drawn, so this is purely cosmetic:
			-- a project whose actual paper exceeds A3, as this one's
			-- custom 584.2 x 378.46mm size does, will have correctly-
			-- placed content extending beyond the default frame
			-- border. Logged so this is visible up front rather than
			-- silently discovered in the GUI:
			if et_drawing_frame.type_distance (sheet.paper_width) > et_drawing_frame.paper_size_A3_x
				or et_drawing_frame.type_distance (sheet.paper_height) > et_drawing_frame.paper_size_A3_y
			then
				log (SEVERITY_WARNING,
					text	=> "sheet paper size" & type_distance_model'image (sheet.paper_width)
						& " x" & type_distance_model'image (sheet.paper_height)
						& " exceeds et_drawing_frame's largest supported size, A3 landscape ("
						& et_drawing_frame.type_distance'image (et_drawing_frame.paper_size_A3_x) & " x"
						& et_drawing_frame.type_distance'image (et_drawing_frame.paper_size_A3_y)
						& ") -- content is still placed correctly, but will extend beyond the "
						& "drawn frame border",
					level	=> log_threshold);
			end if;

			meta_captured := true;
		end build_meta_and_frame;


		procedure walk (node : in type_sheet_node_access) is
			raw : natural;
		begin
			if node = null then
				return;
			end if;

			raw := raw_page_number (node, log_threshold);

			if raw = 0 then
				log (SEVERITY_NOTE,
					text	=> "sheet 0 (" & to_string (node.uuid_path)
						& ") ignored -- no devices/nets converted from it",
					level	=> log_threshold + 1);
			else
				declare
					sheet_num : constant type_sheet := type_sheet (raw);
				begin
					declare
						use ada.strings.unbounded;
						title : constant string := to_string (node.data.title);
					begin
						pac_sheet_titles.include (sheet_titles, sheet_num,
							to_unbounded_string (
								(if title'length > 0 then title else "sheet" & type_sheet'image (sheet_num))));
					end;

					for sym of node.data.placed_symbols loop
						build_device (sym, node, sheet_num);
						build_location_text (sym, sheet_num, node.data.paper_height);
					end loop;

					for strand of node.data.strands loop
						build_net_contribution (sheet_num, strand, node.data.all);
					end loop;

					for txt of node.data.texts loop
						build_free_text (txt, sheet_num, node.data.paper_height);
					end loop;

					build_meta_and_frame (node.data.all);

				exception
					when constraint_error =>
						log (SEVERITY_WARNING,
							text	=> "sheet page" & natural'image (raw) & " out of range -> ignored",
							level	=> log_threshold);
				end;
			end if;

			for child of node.children loop
				walk (child);
			end loop;
		end walk;

	begin
		log (text => "converting KiCad v6 project " & to_string (project.name) & " to native ...",
			level => log_threshold);
		log_indentation_up;

		build_device_models (project.merged_symbols, log_threshold + 1);

		walk (project.root);

		-- Turn the per-sheet titles collected during the walk into
		-- module.frames.descriptions, one entry per sheet number from
		-- 1 to the highest one seen -- see the sheet_titles/
		-- pac_sheet_titles declaration above for why this matters:
		declare
			use ada.strings.unbounded;
			max_sheet : type_sheet := 1;
		begin
			for c in sheet_titles.iterate loop
				if pac_sheet_titles.key (c) > max_sheet then
					max_sheet := pac_sheet_titles.key (c);
				end if;
			end loop;

			for n in 1 .. max_sheet loop
				declare
					c : constant pac_sheet_titles.cursor := sheet_titles.find (n);

					text : constant string := (
						if c /= pac_sheet_titles.no_element
						then to_string (pac_sheet_titles.element (c))
						else "no description");

					sanitized : constant string := sanitize_text_content (text);

					-- type_text_content allows at most text_length_max
					-- characters -- a KiCad title_block title is not
					-- expected to be anywhere near that long, but this
					-- must not raise if one ever is:
					clipped : constant string := sanitized (
						sanitized'first .. sanitized'first - 1 +
							natural'min (sanitized'length, et_text_content.text_length_max));
				begin
					et_drawing_frame.schematic.pac_schematic_descriptions.append (
						module.frames.descriptions,
						(content => et_text_content.to_content (clipped), others => <>));
				end;
			end loop;
		end;

		log_indentation_down;
		return module;
	end convert;


	------------------------------------------------------------------
	-- TO_NATIVE
	------------------------------------------------------------------

	procedure to_native (
		project			: in et_kicad_v6.schematic.type_project;
		log_threshold	: in type_log_level)
	is
		module		: constant type_generic_module := convert (project, log_threshold);
		module_name	: constant type_module_name := to_module_name (to_string (project.name));


		procedure save_device_models (log_threshold : in type_log_level) is
			procedure save_one (c : in pac_device_models.cursor) is
			begin
				et_device_write.write_device (
					file_name		=> pac_device_models.key (c),
					device			=> pac_device_models.element (c),
					log_threshold	=> log_threshold);
			end save_one;
		begin
			pac_device_models.iterate (device_library, save_one'access);
		end save_device_models;


		-- Saves the shared placeholder package model (see
		-- placeholder_variants) so that device model files referencing
		-- it via "package_model libraries/packages/not_assigned" don't
		-- point at a non-existent file:
		procedure save_package_models (log_threshold : in type_log_level) is
			procedure save_one (c : et_package_library.pac_package_models.cursor) is
			begin
				et_package_write.write_package (
					file_name		=> et_package_library.pac_package_models.key (c),
					packge			=> et_package_library.pac_package_models.element (c),
					log_threshold	=> log_threshold);
			end save_one;
		begin
			et_package_library.pac_package_models.iterate (package_library, save_one'access);
		end save_package_models;

	begin
		log (text => "saving native project " & to_string (project.name) & " ...", level => log_threshold);
		log_indentation_up;

		et_project.create_project_directory (
			project_name	=> project.name,
			module_name		=> module_name,
			log_threshold	=> log_threshold + 1);

		declare
			current_working_directory	: constant string := current_directory;
			module_list					: pac_generic_modules.map;
			list_cursor					: pac_generic_modules.cursor;
			inserted					: boolean;
		begin
			pac_generic_modules.insert (
				container	=> module_list,
				key			=> module_name,
				position	=> list_cursor,
				inserted	=> inserted,
				new_item	=> module);

			set_directory (to_string (project.name));

			et_module_write.write_module (
				module_cursor	=> module_list.first,
				log_threshold	=> log_threshold + 1);

			save_device_models (log_threshold + 1);
			save_package_models (log_threshold + 1);

			set_directory (current_working_directory);
		end;

		log_indentation_down;
	end to_native;

end et_kicad_v6_to_native;

-- Soli Deo Gloria

-- For God so loved the world that he gave
-- his one and only Son, that whoever believes in him
-- shall not perish but have eternal life.
-- The Bible, John 3.16
