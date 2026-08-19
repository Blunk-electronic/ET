------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                          KICAD V6 / SCHEMATIC                            --
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

--   For correct displaying set tab width in your editor to 4.

with ada.directories;
with ada.strings.fixed;			use ada.strings.fixed;
with ada.containers.ordered_sets;
with et_kicad_v6.sexp;				use et_kicad_v6.sexp;

package body et_kicad_v6.schematic is

	package sexp renames et_kicad_v6.sexp;


	------------------------------------------------------------------
	-- SMALL HELPERS
	------------------------------------------------------------------

	function library_of (id : in type_lib_id) return string is
		text : constant string := to_string (id);
		p    : constant natural := index (text, ":");
	begin
		if p = 0 then
			return text;
		else
			return text (text'first .. p - 1);
		end if;
	end library_of;


	function symbol_of (id : in type_lib_id) return string is
		text : constant string := to_string (id);
		p    : constant natural := index (text, ":");
	begin
		if p = 0 then
			return text;
		else
			return text (p + 1 .. text'last);
		end if;
	end symbol_of;


	function to_pin_electrical_type (text : in string) return type_pin_electrical_type is
	begin
		if text = "input" then return PIN_INPUT;
		elsif text = "output" then return PIN_OUTPUT;
		elsif text = "bidirectional" then return PIN_BIDIRECTIONAL;
		elsif text = "tri_state" then return PIN_TRI_STATE;
		elsif text = "passive" then return PIN_PASSIVE;
		elsif text = "free" then return PIN_FREE;
		elsif text = "unspecified" then return PIN_UNSPECIFIED;
		elsif text = "power_in" then return PIN_POWER_IN;
		elsif text = "power_out" then return PIN_POWER_OUT;
		elsif text = "open_collector" then return PIN_OPEN_COLLECTOR;
		elsif text = "open_emitter" then return PIN_OPEN_EMITTER;
		elsif text = "no_connect" then return PIN_NO_CONNECT;
		else
			-- CS: not logged (no log_threshold available to every
			-- caller of this pure conversion function) -- every
			-- value in KiCad's published grammar is covered above.
			return PIN_UNSPECIFIED;
		end if;
	end to_pin_electrical_type;


	function to_pin_graphic_style (text : in string) return type_pin_graphic_style is
	begin
		if text = "line" then return STYLE_LINE;
		elsif text = "inverted" then return STYLE_INVERTED;
		elsif text = "clock" then return STYLE_CLOCK;
		elsif text = "inverted_clock" then return STYLE_INVERTED_CLOCK;
		elsif text = "input_low" then return STYLE_INPUT_LOW;
		elsif text = "clock_low" then return STYLE_CLOCK_LOW;
		elsif text = "output_low" then return STYLE_OUTPUT_LOW;
		elsif text = "edge_clock_high" then return STYLE_EDGE_CLOCK_HIGH;
		elsif text = "non_logic" then return STYLE_NON_LOGIC;
		else
			return STYLE_LINE; -- CS: not logged, see to_pin_electrical_type
		end if;
	end to_pin_graphic_style;


	function to_label_shape (text : in string) return type_label_shape is
	begin
		if text = "input" then return SHAPE_INPUT;
		elsif text = "output" then return SHAPE_OUTPUT;
		elsif text = "bidirectional" then return SHAPE_BIDIRECTIONAL;
		elsif text = "tri_state" then return SHAPE_TRI_STATE;
		elsif text = "passive" then return SHAPE_PASSIVE;
		else
			return SHAPE_PASSIVE; -- CS: not logged, see to_pin_electrical_type
		end if;
	end to_label_shape;


	function classify_graphic (tag : in string) return type_symbol_graphic_item_kind is
	begin
		if tag = "polyline" then return GFX_POLYLINE;
		elsif tag = "arc" then return GFX_ARC;
		elsif tag = "circle" then return GFX_CIRCLE;
		elsif tag = "rectangle" then return GFX_RECTANGLE;
		elsif tag = "text" then return GFX_TEXT;
		else return GFX_OTHER;
		end if;
	end classify_graphic;


	-- Splits a sub-unit name like "F00_0_1" into (unit, convert).
	-- The LAST two underscore-separated fields are (unit, convert);
	-- everything before that is the base symbol name, which may
	-- itself legitimately contain underscores:
	procedure parse_unit_convert_suffix (
		sub_name		: in string;
		unit, convert	: out natural)
	is
		last_us, second_last_us : natural := 0;
	begin
		for i in reverse sub_name'range loop
			if sub_name (i) = '_' then
				if last_us = 0 then
					last_us := i;
				else
					second_last_us := i;
					exit;
				end if;
			end if;
		end loop;

		if last_us = 0 or second_last_us = 0 then
			unit := 0;
			convert := 1;
			return;
		end if;

		unit    := natural'value (sub_name (second_last_us + 1 .. last_us - 1));
		convert := natural'value (sub_name (last_us + 1 .. sub_name'last));

	exception
		when others =>
			unit := 0;
			convert := 1;
	end parse_unit_convert_suffix;


	function get_sub_unit (
		symbol			: in type_lib_symbol;
		unit, convert	: in natural)
		return type_symbol_sub_unit
	is
		c : pac_symbol_sub_units.cursor := symbol.sub_units.first;

		function try_match (u, cv : in natural) return pac_symbol_sub_units.cursor is
			cc : pac_symbol_sub_units.cursor := symbol.sub_units.first;
		begin
			while pac_symbol_sub_units.has_element (cc) loop
				if pac_symbol_sub_units.element (cc).key.unit = u
					and then pac_symbol_sub_units.element (cc).key.convert = cv
				then
					return cc;
				end if;

				pac_symbol_sub_units.next (cc);
			end loop;

			return pac_symbol_sub_units.no_element;
		end try_match;

	begin
		c := try_match (unit, convert);

		if not pac_symbol_sub_units.has_element (c) then
			c := try_match (unit, 1);
		end if;

		if not pac_symbol_sub_units.has_element (c) then
			c := try_match (0, 1);
		end if;

		if pac_symbol_sub_units.has_element (c) then
			return pac_symbol_sub_units.element (c);
		else
			return (key => (unit => unit, convert => convert), others => <>);
		end if;
	end get_sub_unit;


	------------------------------------------------------------------
	-- GEOMETRY PARSING PRIMITIVES
	------------------------------------------------------------------

	function parse_xy (n : in sexp.type_node) return type_vector_model is
	begin
		return set (
			x => type_distance_model (sexp.atom_to_real (sexp.get_child (n, 2))),
			y => type_distance_model (sexp.atom_to_real (sexp.get_child (n, 3))));
	end parse_xy;


	function parse_rotation (n : in sexp.type_node) return type_rotation_model is
	begin
		if sexp.child_count (n) >= 4 then
			return type_rotation_model (sexp.atom_to_real (sexp.get_child (n, 4)));
		else
			return 0.0;
		end if;
	end parse_rotation;


	function parse_points (pts_node : in sexp.type_node) return pac_points.vector is
		result	: pac_points.vector;
		nodes	: constant sexp.pac_node_list.vector := sexp.find_all_children (pts_node, "xy");
		c		: sexp.pac_node_list.cursor := nodes.first;
	begin
		while sexp.pac_node_list.has_element (c) loop
			result.append (parse_xy (sexp.pac_node_list.element (c)));
			sexp.pac_node_list.next (c);
		end loop;

		return result;
	end parse_points;


	------------------------------------------------------------------
	-- PROPERTIES
	------------------------------------------------------------------

	-- True if any direct child of n is an unquoted atom whose text
	-- equals text -- used for flag-style blocks like
	-- (pin_names (offset 1.016) hide) where the flag atom's
	-- position shifts depending on which optional sub-blocks (like
	-- "offset") are present, so a fixed child index cannot be
	-- assumed:
	function contains_atom (n : in sexp.type_node; text : in string) return boolean is
	begin
		for i in 1 .. sexp.child_count (n) loop
			declare
				child : constant sexp.type_node := sexp.get_child (n, i);
			begin
				if sexp.kind (child) = sexp.SEXP_ATOM
					and then not sexp.atom_was_quoted (child)
					and then sexp.atom_text (child) = text
				then
					return true;
				end if;
			end;
		end loop;

		return false;
	end contains_atom;


	-- Reads "(justify [left|right] [top|bottom])" out of a node's
	-- "effects" child (n is the property/text node itself, not
	-- effects directly) -- a missing axis, or a missing "justify"/
	-- "effects" node entirely, means that axis is centered, matching
	-- KiCad's own default:
	function parse_justify_h (n : in sexp.type_node) return type_justify_horizontal is
		effects_node : constant sexp.type_node := sexp.find_first_child (n, "effects");
		justify_node : constant sexp.type_node := sexp.find_first_child (effects_node, "justify");
	begin
		if sexp.kind (justify_node) /= sexp.SEXP_LIST then
			return JUSTIFY_H_CENTER;
		elsif contains_atom (justify_node, "left") then
			return JUSTIFY_H_LEFT;
		elsif contains_atom (justify_node, "right") then
			return JUSTIFY_H_RIGHT;
		else
			return JUSTIFY_H_CENTER;
		end if;
	end parse_justify_h;


	function parse_justify_v (n : in sexp.type_node) return type_justify_vertical is
		effects_node : constant sexp.type_node := sexp.find_first_child (n, "effects");
		justify_node : constant sexp.type_node := sexp.find_first_child (effects_node, "justify");
	begin
		if sexp.kind (justify_node) /= sexp.SEXP_LIST then
			return JUSTIFY_V_CENTER;
		elsif contains_atom (justify_node, "top") then
			return JUSTIFY_V_TOP;
		elsif contains_atom (justify_node, "bottom") then
			return JUSTIFY_V_BOTTOM;
		else
			return JUSTIFY_V_CENTER;
		end if;
	end parse_justify_v;


	procedure parse_properties (n : in sexp.type_node; properties : in out pac_properties.map) is
		nodes	: constant sexp.pac_node_list.vector := sexp.find_all_children (n, "property");
		c		: sexp.pac_node_list.cursor := nodes.first;
	begin
		while sexp.pac_node_list.has_element (c) loop
			declare
				p : constant sexp.type_node := sexp.pac_node_list.element (c);
			begin
				if sexp.child_count (p) >= 3 then
					properties.include (
						to_property_name (sexp.atom_text (sexp.get_child (p, 2))),
						to_property_value (sexp.atom_text (sexp.get_child (p, 3))));
				end if;
			end;

			sexp.pac_node_list.next (c);
		end loop;
	end parse_properties;


	-- Same source nodes as parse_properties, but captures each
	-- property's "(at x y rot)" instead of its value -- only
	-- meaningful for a PLACED symbol's own properties (a lib_symbol's
	-- properties carry a nominal library-editor position that has no
	-- bearing on any particular placement):
	procedure parse_property_placements (n : in sexp.type_node; placements : in out pac_property_placements.map) is
		nodes	: constant sexp.pac_node_list.vector := sexp.find_all_children (n, "property");
		c		: sexp.pac_node_list.cursor := nodes.first;
	begin
		while sexp.pac_node_list.has_element (c) loop
			declare
				p		: constant sexp.type_node := sexp.pac_node_list.element (c);
				at_node	: constant sexp.type_node := sexp.find_first_child (p, "at");
			begin
				if sexp.child_count (p) >= 3 and then sexp.kind (at_node) = sexp.SEXP_LIST then
					placements.include (
						to_property_name (sexp.atom_text (sexp.get_child (p, 2))),
						(position	=> parse_xy (at_node),
						 rotation	=> parse_rotation (at_node),
						 justify_h	=> parse_justify_h (p),
						 justify_v	=> parse_justify_v (p)));
				end if;
			end;

			sexp.pac_node_list.next (c);
		end loop;
	end parse_property_placements;


	function get_property (properties : in pac_properties.map; name : in string) return type_property_value is
		c : constant pac_properties.cursor := properties.find (to_property_name (name));
	begin
		if pac_properties.has_element (c) then
			return pac_properties.element (c);
		else
			return to_property_value ("");
		end if;
	end get_property;


	------------------------------------------------------------------
	-- PINS / SUB-UNITS / LIB_SYMBOLS
	------------------------------------------------------------------

	function parse_pin_definition (n : in sexp.type_node) return type_pin is
		result		: type_pin;
		at_node		: constant sexp.type_node := sexp.find_first_child (n, "at");
		length_node	: constant sexp.type_node := sexp.find_first_child (n, "length");
		name_node	: constant sexp.type_node := sexp.find_first_child (n, "name");
		number_node	: constant sexp.type_node := sexp.find_first_child (n, "number");
	begin
		if sexp.child_count (n) >= 2 then
			result.electrical_type := to_pin_electrical_type (sexp.atom_text (sexp.get_child (n, 2)));
		end if;

		if sexp.child_count (n) >= 3 then
			result.graphic_style := to_pin_graphic_style (sexp.atom_text (sexp.get_child (n, 3)));
		end if;

		if sexp.kind (at_node) = sexp.SEXP_LIST then
			result.position := parse_xy (at_node);
			result.orientation := parse_rotation (at_node);
		end if;

		if sexp.kind (length_node) = sexp.SEXP_LIST and then sexp.child_count (length_node) >= 2 then
			result.length := type_distance_model (sexp.atom_to_real (sexp.get_child (length_node, 2)));
		end if;

		if sexp.kind (name_node) = sexp.SEXP_LIST and then sexp.child_count (name_node) >= 2 then
			result.name := to_property_value (sexp.atom_text (sexp.get_child (name_node, 2)));
		end if;

		if sexp.kind (number_node) = sexp.SEXP_LIST and then sexp.child_count (number_node) >= 2 then
			result.number := to_property_value (sexp.atom_text (sexp.get_child (number_node, 2)));
		end if;

		-- result.uuid intentionally left default: lib_symbol pin
		-- DEFINITIONS carry no uuid in the file (only per-placement
		-- pin-number-to-uuid mappings do, and those are not modeled --
		-- see the package description).
		return result;
	end parse_pin_definition;


	function parse_symbol_sub_unit (n : in sexp.type_node) return type_symbol_sub_unit is
		result		: type_symbol_sub_unit;
		sub_name	: constant string := sexp.atom_text (sexp.get_child (n, 2));
	begin
		parse_unit_convert_suffix (sub_name, result.key.unit, result.key.convert);

		-- Children 3 .. N are either pin definitions or graphics
		-- (everything that is not "pin" is treated as an opaque
		-- graphic item -- this is a loader, not a renderer):
		for i in 3 .. sexp.child_count (n) loop
			declare
				child : constant sexp.type_node := sexp.get_child (n, i);
				tag   : constant string := sexp.head (child);
			begin
				if tag = "pin" then
					result.pins.append (parse_pin_definition (child));
				else
					result.graphics.append ((
						item_kind	=> classify_graphic (tag),
						raw			=> sexp.get_child_access (n, i)));
				end if;
			end;
		end loop;

		return result;
	end parse_symbol_sub_unit;


	function parse_lib_symbol (n : in sexp.type_node) return type_lib_symbol is
		result			: type_lib_symbol;
		sub_unit_nodes	: constant sexp.pac_node_list.vector := sexp.find_all_children (n, "symbol");
		c				: sexp.pac_node_list.cursor;
		pin_numbers_n	: constant sexp.type_node := sexp.find_first_child (n, "pin_numbers");
		pin_names_n		: constant sexp.type_node := sexp.find_first_child (n, "pin_names");
		in_bom_n		: constant sexp.type_node := sexp.find_first_child (n, "in_bom");
		on_board_n		: constant sexp.type_node := sexp.find_first_child (n, "on_board");
	begin
		if sexp.child_count (n) >= 2 then
			result.lib_id := to_lib_id (sexp.atom_text (sexp.get_child (n, 2)));
		end if;

		result.is_power := sexp.kind (sexp.find_first_child (n, "power")) = sexp.SEXP_LIST;

		if sexp.kind (pin_numbers_n) = sexp.SEXP_LIST then
			result.pin_numbers_hidden := contains_atom (pin_numbers_n, "hide");
		end if;

		if sexp.kind (pin_names_n) = sexp.SEXP_LIST then
			result.pin_names_hidden := contains_atom (pin_names_n, "hide");
		end if;

		if sexp.kind (in_bom_n) = sexp.SEXP_LIST then
			result.in_bom := sexp.atom_to_yes_no (sexp.get_child (in_bom_n, 2));
		end if;

		if sexp.kind (on_board_n) = sexp.SEXP_LIST then
			result.on_board := sexp.atom_to_yes_no (sexp.get_child (on_board_n, 2));
		end if;

		parse_properties (n, result.properties);

		c := sub_unit_nodes.first;
		while sexp.pac_node_list.has_element (c) loop
			result.sub_units.append (parse_symbol_sub_unit (sexp.pac_node_list.element (c)));
			sexp.pac_node_list.next (c);
		end loop;

		return result;
	end parse_lib_symbol;


	------------------------------------------------------------------
	-- PLACED SYMBOLS
	------------------------------------------------------------------

	function parse_instance_refs (instances_node : in sexp.type_node) return pac_instance_refs.list is
		result			: pac_instance_refs.list;
		project_nodes	: constant sexp.pac_node_list.vector := sexp.find_all_children (instances_node, "project");
		pc				: sexp.pac_node_list.cursor := project_nodes.first;
	begin
		while sexp.pac_node_list.has_element (pc) loop
			declare
				proj_node	: constant sexp.type_node := sexp.pac_node_list.element (pc);
				proj_name	: type_property_value := to_property_value ("");
				path_nodes	: constant sexp.pac_node_list.vector := sexp.find_all_children (proj_node, "path");
				pathc		: sexp.pac_node_list.cursor := path_nodes.first;
			begin
				if sexp.child_count (proj_node) >= 2 then
					proj_name := to_property_value (sexp.atom_text (sexp.get_child (proj_node, 2)));
				end if;

				while sexp.pac_node_list.has_element (pathc) loop
					declare
						path_node		: constant sexp.type_node := sexp.pac_node_list.element (pathc);
						ref				: type_instance_ref;
						reference_node	: constant sexp.type_node := sexp.find_first_child (path_node, "reference");
						unit_node		: constant sexp.type_node := sexp.find_first_child (path_node, "unit");
						value_node		: constant sexp.type_node := sexp.find_first_child (path_node, "value");
						footprint_node	: constant sexp.type_node := sexp.find_first_child (path_node, "footprint");
					begin
						ref.project_name := proj_name;

						if sexp.child_count (path_node) >= 2 then
							ref.path := et_kicad_v6.to_uuid_path (sexp.atom_text (sexp.get_child (path_node, 2)));
						end if;

						if sexp.kind (reference_node) = sexp.SEXP_LIST and then sexp.child_count (reference_node) >= 2 then
							ref.reference := to_property_value (sexp.atom_text (sexp.get_child (reference_node, 2)));
						end if;

						if sexp.kind (unit_node) = sexp.SEXP_LIST and then sexp.child_count (unit_node) >= 2 then
							ref.unit := sexp.atom_to_natural (sexp.get_child (unit_node, 2));
						end if;

						if sexp.kind (value_node) = sexp.SEXP_LIST and then sexp.child_count (value_node) >= 2 then
							ref.value := to_property_value (sexp.atom_text (sexp.get_child (value_node, 2)));
						end if;

						if sexp.kind (footprint_node) = sexp.SEXP_LIST and then sexp.child_count (footprint_node) >= 2 then
							ref.footprint := to_property_value (sexp.atom_text (sexp.get_child (footprint_node, 2)));
						end if;

						result.append (ref);
					end;

					sexp.pac_node_list.next (pathc);
				end loop;
			end;

			sexp.pac_node_list.next (pc);
		end loop;

		return result;
	end parse_instance_refs;


	function parse_placed_symbol (n : in sexp.type_node) return type_placed_symbol is
		result			: type_placed_symbol;
		lib_id_node		: constant sexp.type_node := sexp.find_first_child (n, "lib_id");
		at_node			: constant sexp.type_node := sexp.find_first_child (n, "at");
		unit_node		: constant sexp.type_node := sexp.find_first_child (n, "unit");
		convert_node	: constant sexp.type_node := sexp.find_first_child (n, "convert");
		mirror_node		: constant sexp.type_node := sexp.find_first_child (n, "mirror");
		uuid_node		: constant sexp.type_node := sexp.find_first_child (n, "uuid");
		in_bom_node		: constant sexp.type_node := sexp.find_first_child (n, "in_bom");
		on_board_node	: constant sexp.type_node := sexp.find_first_child (n, "on_board");
		dnp_node		: constant sexp.type_node := sexp.find_first_child (n, "dnp");
		instances_node	: constant sexp.type_node := sexp.find_first_child (n, "instances");
	begin
		if sexp.kind (lib_id_node) = sexp.SEXP_LIST and then sexp.child_count (lib_id_node) >= 2 then
			result.lib_id := to_lib_id (sexp.atom_text (sexp.get_child (lib_id_node, 2)));
		end if;

		if sexp.kind (at_node) = sexp.SEXP_LIST then
			result.position := parse_xy (at_node);
			result.orientation := parse_rotation (at_node);
		end if;

		if sexp.kind (unit_node) = sexp.SEXP_LIST and then sexp.child_count (unit_node) >= 2 then
			result.unit := sexp.atom_to_natural (sexp.get_child (unit_node, 2));
		end if;

		if sexp.kind (convert_node) = sexp.SEXP_LIST and then sexp.child_count (convert_node) >= 2 then
			result.convert := sexp.atom_to_natural (sexp.get_child (convert_node, 2));
		end if;

		if sexp.kind (mirror_node) = sexp.SEXP_LIST and then sexp.child_count (mirror_node) >= 2 then
			declare
				axis : constant string := sexp.atom_text (sexp.get_child (mirror_node, 2));
			begin
				if axis = "x" then
					result.mirror := MIRROR_ALONG_X_AXIS;
				elsif axis = "y" then
					result.mirror := MIRROR_ALONG_Y_AXIS;
				end if;
			end;
		end if;

		if sexp.kind (uuid_node) = sexp.SEXP_LIST and then sexp.child_count (uuid_node) >= 2 then
			result.uuid := to_uuid (sexp.atom_text (sexp.get_child (uuid_node, 2)));
		end if;

		if sexp.kind (in_bom_node) = sexp.SEXP_LIST then
			result.in_bom := sexp.atom_to_yes_no (sexp.get_child (in_bom_node, 2));
		end if;

		if sexp.kind (on_board_node) = sexp.SEXP_LIST then
			result.on_board := sexp.atom_to_yes_no (sexp.get_child (on_board_node, 2));
		end if;

		if sexp.kind (dnp_node) = sexp.SEXP_LIST then
			result.dnp := sexp.atom_to_yes_no (sexp.get_child (dnp_node, 2));
		end if;

		parse_properties (n, result.properties);
		parse_property_placements (n, result.placements);

		if sexp.kind (instances_node) = sexp.SEXP_LIST then
			result.instances := parse_instance_refs (instances_node);
		end if;

		return result;
	end parse_placed_symbol;


	------------------------------------------------------------------
	-- WIRES / BUSES / BUS ENTRIES / JUNCTIONS / NO-CONNECTS
	------------------------------------------------------------------

	function parse_wire (n : in sexp.type_node) return type_wire is
		result		: type_wire;
		pts_node	: constant sexp.type_node := sexp.find_first_child (n, "pts");
		uuid_node	: constant sexp.type_node := sexp.find_first_child (n, "uuid");
	begin
		if sexp.kind (pts_node) = sexp.SEXP_LIST then
			result.points := parse_points (pts_node);
		end if;

		if sexp.kind (uuid_node) = sexp.SEXP_LIST and then sexp.child_count (uuid_node) >= 2 then
			result.uuid := to_uuid (sexp.atom_text (sexp.get_child (uuid_node, 2)));
		end if;

		return result;
	end parse_wire;


	function parse_bus (n : in sexp.type_node) return type_bus is
		result		: type_bus;
		pts_node	: constant sexp.type_node := sexp.find_first_child (n, "pts");
		uuid_node	: constant sexp.type_node := sexp.find_first_child (n, "uuid");
	begin
		if sexp.kind (pts_node) = sexp.SEXP_LIST then
			result.points := parse_points (pts_node);
		end if;

		if sexp.kind (uuid_node) = sexp.SEXP_LIST and then sexp.child_count (uuid_node) >= 2 then
			result.uuid := to_uuid (sexp.atom_text (sexp.get_child (uuid_node, 2)));
		end if;

		return result;
	end parse_bus;


	function parse_bus_entry (n : in sexp.type_node) return type_bus_entry is
		result		: type_bus_entry;
		at_node		: constant sexp.type_node := sexp.find_first_child (n, "at");
		size_node	: constant sexp.type_node := sexp.find_first_child (n, "size");
		uuid_node	: constant sexp.type_node := sexp.find_first_child (n, "uuid");
	begin
		if sexp.kind (at_node) = sexp.SEXP_LIST then
			result.position := parse_xy (at_node);
		end if;

		if sexp.kind (size_node) = sexp.SEXP_LIST then
			result.size := parse_xy (size_node);
		end if;

		if sexp.kind (uuid_node) = sexp.SEXP_LIST and then sexp.child_count (uuid_node) >= 2 then
			result.uuid := to_uuid (sexp.atom_text (sexp.get_child (uuid_node, 2)));
		end if;

		return result;
	end parse_bus_entry;


	function parse_junction (n : in sexp.type_node) return type_junction is
		result			: type_junction;
		at_node			: constant sexp.type_node := sexp.find_first_child (n, "at");
		diameter_node	: constant sexp.type_node := sexp.find_first_child (n, "diameter");
		uuid_node		: constant sexp.type_node := sexp.find_first_child (n, "uuid");
	begin
		if sexp.kind (at_node) = sexp.SEXP_LIST then
			result.position := parse_xy (at_node);
		end if;

		if sexp.kind (diameter_node) = sexp.SEXP_LIST and then sexp.child_count (diameter_node) >= 2 then
			result.diameter := type_distance_model (sexp.atom_to_real (sexp.get_child (diameter_node, 2)));
		end if;

		if sexp.kind (uuid_node) = sexp.SEXP_LIST and then sexp.child_count (uuid_node) >= 2 then
			result.uuid := to_uuid (sexp.atom_text (sexp.get_child (uuid_node, 2)));
		end if;

		return result;
	end parse_junction;


	function parse_no_connect (n : in sexp.type_node) return type_no_connect is
		result		: type_no_connect;
		at_node		: constant sexp.type_node := sexp.find_first_child (n, "at");
		uuid_node	: constant sexp.type_node := sexp.find_first_child (n, "uuid");
	begin
		if sexp.kind (at_node) = sexp.SEXP_LIST then
			result.position := parse_xy (at_node);
		end if;

		if sexp.kind (uuid_node) = sexp.SEXP_LIST and then sexp.child_count (uuid_node) >= 2 then
			result.uuid := to_uuid (sexp.atom_text (sexp.get_child (uuid_node, 2)));
		end if;

		return result;
	end parse_no_connect;


	------------------------------------------------------------------
	-- LABELS AND FREE TEXT
	------------------------------------------------------------------

	function parse_label (n : in sexp.type_node; label_kind : in type_label_kind) return type_label is
		result		: type_label;
		at_node		: constant sexp.type_node := sexp.find_first_child (n, "at");
		shape_node	: constant sexp.type_node := sexp.find_first_child (n, "shape");
		uuid_node	: constant sexp.type_node := sexp.find_first_child (n, "uuid");
	begin
		result.label_kind := label_kind;

		if sexp.child_count (n) >= 2 then
			result.text := to_property_value (sexp.atom_text (sexp.get_child (n, 2)));
		end if;

		if sexp.kind (at_node) = sexp.SEXP_LIST then
			result.position := parse_xy (at_node);
			result.orientation := parse_rotation (at_node);
		end if;

		if sexp.kind (shape_node) = sexp.SEXP_LIST and then sexp.child_count (shape_node) >= 2 then
			result.shape := to_label_shape (sexp.atom_text (sexp.get_child (shape_node, 2)));
		end if;

		if sexp.kind (uuid_node) = sexp.SEXP_LIST and then sexp.child_count (uuid_node) >= 2 then
			result.uuid := to_uuid (sexp.atom_text (sexp.get_child (uuid_node, 2)));
		end if;

		return result;
	end parse_label;


	function parse_free_text (n : in sexp.type_node) return type_free_text is
		result		: type_free_text;
		at_node		: constant sexp.type_node := sexp.find_first_child (n, "at");
		uuid_node	: constant sexp.type_node := sexp.find_first_child (n, "uuid");
	begin
		if sexp.child_count (n) >= 2 then
			result.text := to_property_value (sexp.atom_text (sexp.get_child (n, 2)));
		end if;

		if sexp.kind (at_node) = sexp.SEXP_LIST then
			result.position := parse_xy (at_node);
			result.orientation := parse_rotation (at_node);
		end if;

		result.justify_h := parse_justify_h (n);
		result.justify_v := parse_justify_v (n);

		if sexp.kind (uuid_node) = sexp.SEXP_LIST and then sexp.child_count (uuid_node) >= 2 then
			result.uuid := to_uuid (sexp.atom_text (sexp.get_child (uuid_node, 2)));
		end if;

		return result;
	end parse_free_text;


	------------------------------------------------------------------
	-- SHEET REFERENCES (child sheets, as seen from the parent file)
	------------------------------------------------------------------

	function parse_sheet_ref (
		n				: in sexp.type_node;
		log_threshold	: in type_log_level)
		return type_sheet_ref
	is
		result			: type_sheet_ref;
		at_node			: constant sexp.type_node := sexp.find_first_child (n, "at");
		size_node		: constant sexp.type_node := sexp.find_first_child (n, "size");
		uuid_node		: constant sexp.type_node := sexp.find_first_child (n, "uuid");
		props			: pac_properties.map;
		pin_nodes		: constant sexp.pac_node_list.vector := sexp.find_all_children (n, "pin");
	begin
		if sexp.kind (at_node) = sexp.SEXP_LIST then
			result.position := parse_xy (at_node);
		end if;

		if sexp.kind (size_node) = sexp.SEXP_LIST then
			result.size := parse_xy (size_node);
		end if;

		if sexp.kind (uuid_node) = sexp.SEXP_LIST and then sexp.child_count (uuid_node) >= 2 then
			result.uuid := to_uuid (sexp.atom_text (sexp.get_child (uuid_node, 2)));
		end if;

		parse_properties (n, props);
		result.sheetname := get_property (props, "Sheetname");
		result.sheetfile := get_property (props, "Sheetfile");

		if natural (pin_nodes.length) > 0 then
			log_unknown_key ("sheet", "pin", log_threshold, deferred => true);
		end if;

		return result;
	end parse_sheet_ref;


	------------------------------------------------------------------
	-- STRAND / NET CONNECTIVITY (union-find over shared coordinates)
	------------------------------------------------------------------

	procedure build_strands (sheet : in out type_sheet_data; log_threshold : in type_log_level) is

		package pac_point_index is new ada.containers.ordered_maps
			(key_type => type_vector_model, element_type => positive);

		package pac_parent is new ada.containers.vectors (positive, positive);

		package pac_group_strands is new ada.containers.ordered_maps
			(key_type => positive, element_type => type_strand);

		point_index	: pac_point_index.map;
		parent		: pac_parent.vector;
		groups		: pac_group_strands.map;

		function get_or_add_index (p : in type_vector_model) return positive is
			c : constant pac_point_index.cursor := point_index.find (p);
		begin
			if pac_point_index.has_element (c) then
				return pac_point_index.element (c);
			else
				declare
					new_index : constant positive := natural (parent.length) + 1;
				begin
					point_index.insert (p, new_index);
					parent.append (new_index);
					return new_index;
				end;
			end if;
		end get_or_add_index;

		function find_root (i : in positive) return positive is
			current : positive := i;
			walker  : positive;
		begin
			while parent (current) /= current loop
				current := parent (current);
			end loop;

			walker := i;
			while parent (walker) /= current loop
				declare
					next_walker : constant positive := parent (walker);
				begin
					parent.replace_element (walker, current);
					walker := next_walker;
				end;
			end loop;

			return current;
		end find_root;

		procedure union_points (a, b : in type_vector_model) is
			ra : constant positive := find_root (get_or_add_index (a));
			rb : constant positive := find_root (get_or_add_index (b));
		begin
			if ra /= rb then
				parent.replace_element (ra, rb);
			end if;
		end union_points;

		-- True if p lies on the segment a-b: an exact endpoint always
		-- counts; otherwise only the axis-aligned (purely horizontal
		-- or vertical) case is handled, which covers virtually every
		-- wire in a hand-drawn schematic. A point on a genuinely
		-- diagonal segment, away from either of its endpoints, is not
		-- found this way:
		function point_on_segment (p, a, b : in type_vector_model) return boolean is
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


		-- Finds the union-find root of the strand containing point p,
		-- either because p is itself a registered vertex, or because
		-- it lies somewhere along a wire/bus segment whose vertices
		-- are registered (most labels sit mid-segment, not on a
		-- vertex -- looking up registered vertices alone would miss
		-- the vast majority of them). found is false if neither
		-- applies (e.g. a label placed on a pin rather than a wire):
		procedure find_segment_root (
			p		: in type_vector_model;
			root	: out positive;
			found	: out boolean)
		is
			pic : constant pac_point_index.cursor := point_index.find (p);
		begin
			root  := 1;
			found := false;

			if pac_point_index.has_element (pic) then
				root  := find_root (pac_point_index.element (pic));
				found := true;
				return;
			end if;

			for w of sheet.wires loop
				if natural (w.points.length) >= 2 then
					for i in w.points.first_index .. w.points.last_index - 1 loop
						if point_on_segment (p, w.points (i), w.points (i + 1)) then
							root  := find_root (get_or_add_index (w.points (i)));
							found := true;
							return;
						end if;
					end loop;
				end if;
			end loop;

			for b of sheet.buses loop
				if natural (b.points.length) >= 2 then
					for i in b.points.first_index .. b.points.last_index - 1 loop
						if point_on_segment (p, b.points (i), b.points (i + 1)) then
							root  := find_root (get_or_add_index (b.points (i)));
							found := true;
							return;
						end if;
					end loop;
				end if;
			end loop;
		end find_segment_root;


		procedure union_chain (points : in pac_points.vector) is
			dummy : positive;
			pragma unreferenced (dummy);
		begin
			for i in points.first_index .. points.last_index loop
				dummy := get_or_add_index (points (i));
			end loop;

			if natural (points.length) >= 2 then
				for i in points.first_index .. points.last_index - 1 loop
					union_points (points (i), points (i + 1));
				end loop;
			end if;
		end union_chain;

		wc : pac_wires.cursor := sheet.wires.first;
		bc : pac_buses.cursor := sheet.buses.first;
		jc : pac_junctions.cursor := sheet.junctions.first;
		lc : pac_labels.cursor;

	begin
		-- Pass 1: union every wire's / bus's own point chain:
		while pac_wires.has_element (wc) loop
			union_chain (pac_wires.element (wc).points);
			pac_wires.next (wc);
		end loop;

		while pac_buses.has_element (bc) loop
			union_chain (pac_buses.element (bc).points);
			pac_buses.next (bc);
		end loop;

		-- Sanity check: every junction should coincide with a
		-- wire/bus endpoint already registered above. See the "CS"
		-- note on connectivity in the package spec -- true midpoint
		-- T-touches are not yet detected:
		while pac_junctions.has_element (jc) loop
			if not point_index.contains (pac_junctions.element (jc).position) then
				log (SEVERITY_WARNING,
					"junction at a coordinate not matching any wire/bus endpoint -- "
					& "possible unmodeled T-touch connectivity",
					level => log_threshold);
			end if;

			pac_junctions.next (jc);
		end loop;

		-- Pass 2: group every registered point by its union-find root:
		declare
			pc : pac_point_index.cursor := point_index.first;
		begin
			while pac_point_index.has_element (pc) loop
				declare
					p	 : constant type_vector_model := pac_point_index.key (pc);
					root : constant positive := find_root (pac_point_index.element (pc));
					gc	 : constant pac_group_strands.cursor := groups.find (root);
				begin
					if pac_group_strands.has_element (gc) then
						declare
							s : type_strand := pac_group_strands.element (gc);
						begin
							s.points.append (p);
							groups.replace_element (gc, s);
						end;
					else
						declare
							s : type_strand;
						begin
							s.points.append (p);
							groups.insert (root, s);
						end;
					end if;
				end;

				pac_point_index.next (pc);
			end loop;
		end;

		-- Pass 3: attach every label to the strand of whichever
		-- registered point or wire/bus segment it sits on (see
		-- find_segment_root). A label matching neither (e.g. one
		-- placed on a pin rather than a wire) is simply not attached
		-- -- not logged, this is routine:
		lc := sheet.labels.first;
		while pac_labels.has_element (lc) loop
			declare
				lbl			: constant type_label := pac_labels.element (lc);
				root		: positive;
				root_found	: boolean;
			begin
				find_segment_root (lbl.position, root, root_found);

				if root_found then
					declare
						gc : constant pac_group_strands.cursor := groups.find (root);
					begin
						if pac_group_strands.has_element (gc) then
							declare
								s : type_strand := pac_group_strands.element (gc);
							begin
								s.labels.append (lbl);
								groups.replace_element (gc, s);
							end;
						end if;
					end;
				end if;
			end;

			pac_labels.next (lc);
		end loop;

		-- Publish:
		declare
			gc : pac_group_strands.cursor := groups.first;
		begin
			while pac_group_strands.has_element (gc) loop
				sheet.strands.append (pac_group_strands.element (gc));
				pac_group_strands.next (gc);
			end loop;
		end;
	end build_strands;


	------------------------------------------------------------------
	-- ENTRY POINT: read_sheet_file
	------------------------------------------------------------------

	function is_known_top_level_tag (tag : in string) return boolean is
	begin
		return tag = "version" or tag = "generator" or tag = "uuid" or tag = "paper"
			or tag = "title_block" or tag = "lib_symbols" or tag = "sheet" or tag = "symbol"
			or tag = "wire" or tag = "bus" or tag = "bus_entry" or tag = "junction"
			or tag = "no_connect" or tag = "label" or tag = "global_label"
			or tag = "hierarchical_label" or tag = "text" or tag = "bus_alias"
			or tag = "sheet_instances" or tag = "symbol_instances";
	end is_known_top_level_tag;


	-- Standalone graphics drawn directly on a sheet (as opposed to
	-- inside a symbol/lib_symbol definition) -- cosmetic annotations
	-- with no electrical meaning, e.g. a hand-drawn divider line.
	-- CS: STUB, same treatment as symbol-body graphics (see
	-- type_symbol_graphic_item) -- not modeled, since this is a
	-- loader, not a renderer, but acknowledged rather than treated
	-- as a genuinely unexpected tag. Confirmed present (polyline) in
	-- the reference project's pg_79/pg_80.kicad_sch:
	function is_deferred_top_level_graphic_tag (tag : in string) return boolean is
	begin
		return tag = "polyline" or tag = "rectangle" or tag = "circle"
			or tag = "arc" or tag = "bitmap" or tag = "polygon";
	end is_deferred_top_level_graphic_tag;


	function read_sheet_file (
		file_path		: in string;
		log_threshold	: in type_log_level)
		return type_sheet_data
	is
		result	: type_sheet_data;
		root	: constant sexp.type_node := sexp.parse_file (file_path);

		version_node		: constant sexp.type_node := sexp.find_first_child (root, "version");
		generator_node		: constant sexp.type_node := sexp.find_first_child (root, "generator");
		uuid_node			: constant sexp.type_node := sexp.find_first_child (root, "uuid");
		paper_node			: constant sexp.type_node := sexp.find_first_child (root, "paper");
		title_block_node	: constant sexp.type_node := sexp.find_first_child (root, "title_block");
		lib_symbols_node	: constant sexp.type_node := sexp.find_first_child (root, "lib_symbols");

		sheet_nodes			: constant sexp.pac_node_list.vector := sexp.find_all_children (root, "sheet");
		symbol_nodes		: constant sexp.pac_node_list.vector := sexp.find_all_children (root, "symbol");
		wire_nodes			: constant sexp.pac_node_list.vector := sexp.find_all_children (root, "wire");
		bus_nodes			: constant sexp.pac_node_list.vector := sexp.find_all_children (root, "bus");
		bus_entry_nodes		: constant sexp.pac_node_list.vector := sexp.find_all_children (root, "bus_entry");
		junction_nodes		: constant sexp.pac_node_list.vector := sexp.find_all_children (root, "junction");
		no_connect_nodes	: constant sexp.pac_node_list.vector := sexp.find_all_children (root, "no_connect");
		label_nodes			: constant sexp.pac_node_list.vector := sexp.find_all_children (root, "label");
		global_label_nodes	: constant sexp.pac_node_list.vector := sexp.find_all_children (root, "global_label");
		hier_label_nodes	: constant sexp.pac_node_list.vector := sexp.find_all_children (root, "hierarchical_label");
		text_nodes			: constant sexp.pac_node_list.vector := sexp.find_all_children (root, "text");
		bus_alias_nodes		: constant sexp.pac_node_list.vector := sexp.find_all_children (root, "bus_alias");

		c : sexp.pac_node_list.cursor;
	begin
		if sexp.head (root) /= "kicad_sch" then
			log (SEVERITY_WARNING,
				"file " & file_path & " does not start with (kicad_sch ...) -- got '"
				& sexp.head (root) & "'",
				level => log_threshold);
		end if;

		result.file_path := to_property_value (ada.directories.full_name (file_path));

		if sexp.kind (version_node) = sexp.SEXP_LIST and then sexp.child_count (version_node) >= 2 then
			result.version := sexp.atom_to_natural (sexp.get_child (version_node, 2));
		end if;

		if sexp.kind (generator_node) = sexp.SEXP_LIST and then sexp.child_count (generator_node) >= 2 then
			result.generator := to_property_value (sexp.atom_text (sexp.get_child (generator_node, 2)));
		end if;

		if sexp.kind (uuid_node) = sexp.SEXP_LIST and then sexp.child_count (uuid_node) >= 2 then
			result.uuid := to_uuid (sexp.atom_text (sexp.get_child (uuid_node, 2)));
		end if;

		if sexp.kind (paper_node) = sexp.SEXP_LIST and then sexp.child_count (paper_node) >= 2 then
			result.paper_size := to_property_value (sexp.atom_text (sexp.get_child (paper_node, 2)));

			if sexp.child_count (paper_node) >= 4 then
				result.paper_width  := type_distance_model (sexp.atom_to_real (sexp.get_child (paper_node, 3)));
				result.paper_height := type_distance_model (sexp.atom_to_real (sexp.get_child (paper_node, 4)));
			end if;
		end if;

		if sexp.kind (title_block_node) = sexp.SEXP_LIST then
			declare
				title_node : constant sexp.type_node := sexp.find_first_child (title_block_node, "title");
				date_node  : constant sexp.type_node := sexp.find_first_child (title_block_node, "date");
				rev_node   : constant sexp.type_node := sexp.find_first_child (title_block_node, "rev");

				comment_nodes : constant sexp.pac_node_list.vector :=
					sexp.find_all_children (title_block_node, "comment");
				cc : sexp.pac_node_list.cursor := comment_nodes.first;
			begin
				if sexp.kind (title_node) = sexp.SEXP_LIST and then sexp.child_count (title_node) >= 2 then
					result.title := to_property_value (sexp.atom_text (sexp.get_child (title_node, 2)));
				end if;

				if sexp.kind (date_node) = sexp.SEXP_LIST and then sexp.child_count (date_node) >= 2 then
					result.date := to_property_value (sexp.atom_text (sexp.get_child (date_node, 2)));
				end if;

				if sexp.kind (rev_node) = sexp.SEXP_LIST and then sexp.child_count (rev_node) >= 2 then
					result.revision := to_property_value (sexp.atom_text (sexp.get_child (rev_node, 2)));
				end if;

				-- (comment N "text") -- N (child 2) selects which of
				-- the four slots; text is child 3:
				while sexp.pac_node_list.has_element (cc) loop
					declare
						cn : constant sexp.type_node := sexp.pac_node_list.element (cc);
					begin
						if sexp.child_count (cn) >= 3 then
							declare
								n	 : constant natural := sexp.atom_to_natural (sexp.get_child (cn, 2));
								text : constant type_property_value :=
									to_property_value (sexp.atom_text (sexp.get_child (cn, 3)));
							begin
								case n is
									when 1 => result.comment_1 := text;
									when 2 => result.comment_2 := text;
									when 3 => result.comment_3 := text;
									when 4 => result.comment_4 := text;
									when others =>
										log_unknown_key (
											context			=> "title_block comment",
											key				=> natural'image (n),
											log_threshold	=> log_threshold,
											deferred		=> true);
								end case;
							end;
						end if;
					end;

					sexp.pac_node_list.next (cc);
				end loop;
			end;
		end if;

		if sexp.kind (lib_symbols_node) = sexp.SEXP_LIST then
			declare
				lib_symbol_nodes : constant sexp.pac_node_list.vector :=
					sexp.find_all_children (lib_symbols_node, "symbol");
				lc : sexp.pac_node_list.cursor := lib_symbol_nodes.first;
			begin
				while sexp.pac_node_list.has_element (lc) loop
					declare
						sym : constant type_lib_symbol := parse_lib_symbol (sexp.pac_node_list.element (lc));
					begin
						result.symbols.include (sym.lib_id, sym);
					end;

					sexp.pac_node_list.next (lc);
				end loop;
			end;
		end if;

		c := sheet_nodes.first;
		while sexp.pac_node_list.has_element (c) loop
			result.child_sheets.append (parse_sheet_ref (sexp.pac_node_list.element (c), log_threshold));
			sexp.pac_node_list.next (c);
		end loop;

		c := symbol_nodes.first;
		while sexp.pac_node_list.has_element (c) loop
			result.placed_symbols.append (parse_placed_symbol (sexp.pac_node_list.element (c)));
			sexp.pac_node_list.next (c);
		end loop;

		c := wire_nodes.first;
		while sexp.pac_node_list.has_element (c) loop
			result.wires.append (parse_wire (sexp.pac_node_list.element (c)));
			sexp.pac_node_list.next (c);
		end loop;

		c := bus_nodes.first;
		while sexp.pac_node_list.has_element (c) loop
			result.buses.append (parse_bus (sexp.pac_node_list.element (c)));
			sexp.pac_node_list.next (c);
		end loop;

		c := bus_entry_nodes.first;
		while sexp.pac_node_list.has_element (c) loop
			result.bus_entries.append (parse_bus_entry (sexp.pac_node_list.element (c)));
			sexp.pac_node_list.next (c);
		end loop;

		c := junction_nodes.first;
		while sexp.pac_node_list.has_element (c) loop
			result.junctions.append (parse_junction (sexp.pac_node_list.element (c)));
			sexp.pac_node_list.next (c);
		end loop;

		c := no_connect_nodes.first;
		while sexp.pac_node_list.has_element (c) loop
			result.no_connects.append (parse_no_connect (sexp.pac_node_list.element (c)));
			sexp.pac_node_list.next (c);
		end loop;

		c := label_nodes.first;
		while sexp.pac_node_list.has_element (c) loop
			result.labels.append (parse_label (sexp.pac_node_list.element (c), LABEL_LOCAL));
			sexp.pac_node_list.next (c);
		end loop;

		c := global_label_nodes.first;
		while sexp.pac_node_list.has_element (c) loop
			result.labels.append (parse_label (sexp.pac_node_list.element (c), LABEL_GLOBAL));
			sexp.pac_node_list.next (c);
		end loop;

		c := hier_label_nodes.first;
		while sexp.pac_node_list.has_element (c) loop
			result.labels.append (parse_label (sexp.pac_node_list.element (c), LABEL_HIERARCHICAL));
			sexp.pac_node_list.next (c);
		end loop;

		c := text_nodes.first;
		while sexp.pac_node_list.has_element (c) loop
			result.texts.append (parse_free_text (sexp.pac_node_list.element (c)));
			sexp.pac_node_list.next (c);
		end loop;

		if natural (bus_alias_nodes.length) > 0 then
			log_unknown_key ("kicad_sch", "bus_alias", log_threshold, deferred => true);
		end if;

		-- Coverage check: flag any top-level tag not among the ones
		-- explicitly handled above (skip index 1, the file's own
		-- "kicad_sch" tag atom):
		for i in 2 .. sexp.child_count (root) loop
			declare
				tag : constant string := sexp.head (sexp.get_child (root, i));
			begin
				if is_deferred_top_level_graphic_tag (tag) then
					log_unknown_key ("kicad_sch", tag, log_threshold, deferred => true);
				elsif not is_known_top_level_tag (tag) then
					log_unknown_key ("kicad_sch", tag, log_threshold);
				end if;
			end;
		end loop;

		build_strands (result, log_threshold);

		return result;
	end read_sheet_file;


	------------------------------------------------------------------
	-- ENTRY POINT: import_design
	------------------------------------------------------------------

	package pac_path_set is new ada.containers.ordered_sets (type_property_value);


	-- Relative Sheetfile values resolve relative to the REFERENCING
	-- sheet's own containing directory (KiCad's own convention) --
	-- not independently exercisable against the reference project
	-- since all of its sheets sit flat in one directory, but this
	-- generalizes correctly for a project that does use
	-- subdirectories:
	function resolve_relative (base_dir : in string; sheetfile : in string) return string is
	begin
		if sheetfile'length > 0 and then sheetfile (sheetfile'first) = '/' then
			return sheetfile;
		else
			return ada.directories.compose (base_dir, sheetfile);
		end if;
	end resolve_relative;


	-- Loads file_path and everything it (recursively) references.
	-- parent_uuid_path is the uuid path up to but NOT including this
	-- sheet's own uuid (which is only known once the file itself has
	-- been parsed) -- see the package spec's cycle-detection notes:
	function load_recursive (
		file_path			: in string;
		parent_uuid_path	: in pac_uuid_path.list;
		parent				: in type_sheet_node_access;
		on_stack			: in out pac_path_set.set;
		project_data		: in out type_project;
		log_threshold		: in type_log_level)
		return type_sheet_node_access
	is
		resolved	: constant string := ada.directories.full_name (file_path);
		resolved_pv	: constant type_property_value := to_property_value (resolved);
		data		: type_sheet_data_access;
		node		: type_sheet_node_access;
		this_uuid_path : pac_uuid_path.list := parent_uuid_path;
	begin
		if on_stack.contains (resolved_pv) then
			raise sheet_cycle_error with
				"sheet cycle detected: " & resolved & " is its own ancestor (path so far: "
				& et_kicad_v6.to_string (parent_uuid_path) & ")";
		end if;

		on_stack.insert (resolved_pv);

		declare
			cache_c : constant pac_sheet_data_by_path.cursor := project_data.file_cache.find (resolved_pv);
		begin
			if pac_sheet_data_by_path.has_element (cache_c) then
				data := pac_sheet_data_by_path.element (cache_c);
			else
				log (text => "reading " & resolved, level => log_threshold + 1);

				data := new type_sheet_data'(read_sheet_file (resolved, log_threshold + 2));
				project_data.file_cache.insert (resolved_pv, data);

				-- Fold this sheet's own symbols into the project-wide
				-- map, first-definition-wins:
				declare
					sc : pac_lib_symbols.cursor := data.symbols.first;
				begin
					while pac_lib_symbols.has_element (sc) loop
						if not project_data.merged_symbols.contains (pac_lib_symbols.key (sc)) then
							project_data.merged_symbols.insert (pac_lib_symbols.key (sc), pac_lib_symbols.element (sc));
						end if;

						pac_lib_symbols.next (sc);
					end loop;
				end;
			end if;
		end;

		this_uuid_path.append (data.uuid);

		node := new type_sheet_node'(
			uuid_path	=> this_uuid_path,
			data		=> data,
			parent		=> parent,
			children	=> pac_sheet_node_children.empty_vector,
			page		=> to_property_value (""));

		declare
			base_dir : constant string := ada.directories.containing_directory (resolved);
			rc		 : pac_sheet_refs.cursor := data.child_sheets.first;
		begin
			while pac_sheet_refs.has_element (rc) loop
				declare
					ref			: constant type_sheet_ref := pac_sheet_refs.element (rc);
					child_path	: constant string := resolve_relative (base_dir, to_string (ref.sheetfile));
				begin
					node.children.append (
						load_recursive (child_path, this_uuid_path, node, on_stack, project_data, log_threshold));
				end;

				pac_sheet_refs.next (rc);
			end loop;
		end;

		on_stack.delete (resolved_pv);

		return node;
	end load_recursive;


	-- Walks the whole tree looking for the node whose uuid_path
	-- renders as path_text (as found in the root's own
	-- (sheet_instances (path "..." (page "..")))) block), and sets
	-- its page field. A path_text matching no node is silently
	-- ignored (a sheet present in sheet_instances but not reachable
	-- via any (sheet ...) reference would be unusual and is not
	-- expected, not worth failing the whole import over):
	procedure apply_page (
		node		: in type_sheet_node_access;
		path_text	: in string;
		page		: in type_property_value)
	is
	begin
		if node = null then
			return;
		end if;

		if et_kicad_v6.to_string (node.uuid_path) = path_text then
			node.page := page;
		end if;

		for i in node.children.first_index .. node.children.last_index loop
			apply_page (node.children (i), path_text, page);
		end loop;
	end apply_page;


	procedure resolve_pages (
		project_data	: in out type_project;
		root_sexp		: in sexp.type_node;
		log_threshold	: in type_log_level)
	is
		si_node : constant sexp.type_node := sexp.find_first_child (root_sexp, "sheet_instances");
	begin
		if sexp.kind (si_node) /= sexp.SEXP_LIST then
			log (SEVERITY_WARNING, "root sheet has no (sheet_instances ...) block -- page numbers left unresolved",
				level => log_threshold);
			return;
		end if;

		declare
			path_nodes	: constant sexp.pac_node_list.vector := sexp.find_all_children (si_node, "path");
			pc			: sexp.pac_node_list.cursor := path_nodes.first;
		begin
			while sexp.pac_node_list.has_element (pc) loop
				declare
					pn			: constant sexp.type_node := sexp.pac_node_list.element (pc);
					page_node	: constant sexp.type_node := sexp.find_first_child (pn, "page");
					path_text	: type_property_value := to_property_value ("");
					page_text	: type_property_value := to_property_value ("");
				begin
					if sexp.child_count (pn) >= 2 then
						path_text := to_property_value (sexp.atom_text (sexp.get_child (pn, 2)));
					end if;

					if sexp.kind (page_node) = sexp.SEXP_LIST and then sexp.child_count (page_node) >= 2 then
						page_text := to_property_value (sexp.atom_text (sexp.get_child (page_node, 2)));
					end if;

					apply_page (project_data.root, to_string (path_text), page_text);
				end;

				sexp.pac_node_list.next (pc);
			end loop;
		end;
	end resolve_pages;


	-- Folds every strand carrying at least one global label into
	-- project_data.merged_nets, keyed by that label's text (the
	-- first global label found on a strand, if it happens to carry
	-- more than one with different text -- unusual, not expected).
	-- Local/hierarchical-only strands stay reachable solely via
	-- their owning type_sheet_data.strands, never merged here:
	procedure merge_nets (project_data : in out type_project) is
		fc : pac_sheet_data_by_path.cursor := project_data.file_cache.first;
	begin
		while pac_sheet_data_by_path.has_element (fc) loop
			declare
				data : constant type_sheet_data_access := pac_sheet_data_by_path.element (fc);
				sc	 : pac_strands.cursor := data.strands.first;
			begin
				while pac_strands.has_element (sc) loop
					declare
						strand	 : constant type_strand := pac_strands.element (sc);
						lc		 : pac_labels.cursor := strand.labels.first;
						net_name : type_property_value := to_property_value ("");
						found	 : boolean := false;
					begin
						while pac_labels.has_element (lc) and then not found loop
							if pac_labels.element (lc).label_kind = LABEL_GLOBAL then
								net_name := pac_labels.element (lc).text;
								found := true;
							end if;

							pac_labels.next (lc);
						end loop;

						if found then
							declare
								existing_c : constant pac_nets.cursor := project_data.merged_nets.find (net_name);
							begin
								if pac_nets.has_element (existing_c) then
									declare
										lst : pac_strands.list := pac_nets.element (existing_c);
									begin
										lst.append (strand);
										project_data.merged_nets.replace_element (existing_c, lst);
									end;
								else
									declare
										lst : pac_strands.list;
									begin
										lst.append (strand);
										project_data.merged_nets.insert (net_name, lst);
									end;
								end if;
							end;
						end if;
					end;

					pac_strands.next (sc);
				end loop;
			end;

			pac_sheet_data_by_path.next (fc);
		end loop;
	end merge_nets;


	function import_design (
		project				: in et_project_name.type_project_name;
		project_directory	: in string;
		log_threshold		: in type_log_level)
		return type_project
	is
		result		: type_project;
		on_stack	: pac_path_set.set;
		root_file	: constant string :=
			ada.directories.compose (project_directory, et_project_name.to_string (project) & ".kicad_sch");
		empty_uuid_path : pac_uuid_path.list;
	begin
		result.name := project;

		log (text => "importing KiCad v6 project " & et_project_name.to_string (project)
			& " from " & project_directory, level => log_threshold, console => true);
		log_indentation_up;

		result.root := load_recursive (
			root_file, empty_uuid_path, null, on_stack, result, log_threshold);

		log (text => "resolving sheet page numbers ...", level => log_threshold + 1);
		resolve_pages (result, sexp.parse_file (root_file), log_threshold + 1);

		log (text => "merging global nets ...", level => log_threshold + 1);
		merge_nets (result);

		log_indentation_down;

		return result;
	end import_design;


end et_kicad_v6.schematic;

-- Soli Deo Gloria

-- For God so loved the world that he gave
-- his one and only Son, that whoever believes in him
-- shall not perish but have eternal life.
-- The Bible, John 3.16
