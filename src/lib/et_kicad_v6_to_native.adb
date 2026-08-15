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

with et_kicad_v6;						use et_kicad_v6;

with et_project_name;					use et_project_name;
with et_project;
with et_module_names;					use et_module_names;
with et_module_write;
with et_generic_modules;					use et_generic_modules;

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
with et_object_status;					use et_object_status;
with et_device_write;
with et_package_write;

with et_symbol_model;					use et_symbol_model;
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

with et_nets;
with et_net_strands;
with et_net_segment;						use et_net_segment;
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
		-- type_port_length is range 2.0 .. 20.0 -- KiCad pin lengths
		-- outside that range (0.0 for a hidden power pin, or anything
		-- else out of bounds) fall back to the default rather than
		-- raising a range check failure:
		base : constant type_port_general := (
			position	=> pin.position,
			length		=> (if pin.length in type_port_length then pin.length else port_length_default),
			rotation	=> normalize_rotation (pin.orientation));

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


	-- Resolves a sheet node's page number (as filled in by import_design
	-- from the root's sheet_instances block) to a native sheet number.
	-- Defaults to 1 if unresolved or non-numeric:
	function sheet_number_of (
		node			: in type_sheet_node_access;
		log_threshold	: in type_log_level)
		return type_sheet
	is
		text : constant string := to_string (node.page);
	begin
		if text'length = 0 then
			return 1;
		end if;

		return type_sheet (natural'value (text));

	exception
		when others =>
			log (SEVERITY_WARNING,
				text	=> "sheet page number '" & text & "' not numeric -> defaulting to 1",
				level	=> log_threshold);
			return 1;
	end sheet_number_of;


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

			for u of unit_numbers loop
				declare
					sub			: constant type_symbol_sub_unit := get_sub_unit (sym, u, 1);
					ports		: pac_symbol_ports.map;
					unit_name	: constant et_unit_name.type_unit_name :=
						et_unit_name.to_unit_name (trim (natural'image (u), left));

					unit_cursor	: pac_units_internal.cursor;
					unit_inserted : boolean;
				begin
					build_ports (sub.pins, sym.lib_id, log_threshold, ports);

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
										shapes		=> (others => <>),
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
										shapes			=> (others => <>),
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
					point		=> sym.position,
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
						new_unit := (
							appearance		=> APPEARANCE_PCB,
							position		=> unit_position,
							mirror_status	=> sym.mirror,
							status			=> object_status_default,
							placeholders	=> (others => <>));
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
							purpose			=> et_device_purpose.empty_purpose,
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

			procedure collect_from_wire (pts : in pac_points.vector) is
			begin
				if pts.length < 2 then
					return;
				end if;

				for i in pts.first_index .. pts.last_index - 1 loop
					if pac_points.contains (strand.points, pts (i)) then
						pac_net_segments.append (segments, to_net_segment (pts (i), pts (i + 1)));
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


		procedure walk (node : in type_sheet_node_access) is
			sheet_num : type_sheet;
		begin
			if node = null then
				return;
			end if;

			sheet_num := sheet_number_of (node, log_threshold);

			for sym of node.data.placed_symbols loop
				build_device (sym, node, sheet_num);
			end loop;

			for strand of node.data.strands loop
				build_net_contribution (sheet_num, strand, node.data.all);
			end loop;

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
