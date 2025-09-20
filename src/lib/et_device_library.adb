------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                           DEVICE LIBRARY                                 --
--                                                                          --
--                              B o d y                                     --
--                                                                          --
-- Copyright (C) 2017 - 2024                                                --
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

--   For correct displaying set tab width in your edtior to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--

with ada.text_io;				use ada.text_io;

with ada.characters;			use ada.characters;
with ada.characters.latin_1;	use ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;

with ada.strings; 				use ada.strings;
with ada.strings.fixed; 		use ada.strings.fixed;

with et_string_processing;		use et_string_processing;
with et_exceptions;				use et_exceptions;

with et_packages;


package body et_device_library is




	--function hash_device_model (
		--model	: in pac_device_model_file.bounded_string)
		--return hash_type
	--is 
	--begin
		--return ada.strings.hash (to_string (model));
	--end hash_device_model;


	--function equivalent_models (
		--d1, d2 : in type_device_model)
		--return boolean
	--is begin
		--return d1 = d2;
	--end equivalent_models;


	function get_prefix (
		cursor	: in pac_devices_lib.cursor)
		return pac_device_prefix.bounded_string
	is begin
		return element (cursor).prefix;
	end;


	

	function get_device_model_file (
		cursor	: in pac_devices_lib.cursor)
		return pac_device_model_file.bounded_string
	is begin
		return key (cursor);
	end;


	

	function get_device_model_cursor (
		model : in pac_device_model_file.bounded_string)
		return pac_devices_lib.cursor 
	is
		cursor : pac_devices_lib.cursor := find (device_library, model);
	begin
		return cursor;
	end;

	

	

	
	function is_real (
		device_cursor : in pac_devices_lib.cursor)
		return boolean
	is begin
		case element (device_cursor).appearance is
			when APPEARANCE_VIRTUAL => return false;
			when APPEARANCE_PCB => return true;
		end case;
	end is_real;

	


	
	
	function provides_unit (
		device_cursor	: in pac_devices_lib.cursor;
		unit_name		: in pac_unit_name.bounded_string)
		return boolean
	is
		found : boolean := false;
		
		procedure query_internal (
			model	: in pac_device_model_file.bounded_string;
			device	: in type_device_model)
		is begin
			if device.units_internal.contains (unit_name) then
				found := true;
			end if;
		end query_internal;

		procedure query_external (
			model	: in pac_device_model_file.bounded_string;
			device	: in type_device_model)
		is begin
			if device.units_external.contains (unit_name) then
				found := true;
			end if;
		end query_external;
		
	begin -- provided_unit
		-- First search among internal units.
		-- If not found there, search among external units.
		
		pac_devices_lib.query_element (device_cursor, query_internal'access);

		if not found then
			pac_devices_lib.query_element (device_cursor, query_external'access);
		end if;
		
		return found;
	end provides_unit;

	

	
	function get_first_unit (
		device_cursor : in pac_devices_lib.cursor) 
		return type_device_units
	is
		cursors : type_device_units; -- to be returned
		
		use pac_units_internal;
		use pac_units_external;

		
		procedure query_units (
			device_name	: in pac_device_model_file.bounded_string;
			device		: in type_device_model) 
		is

			function first_internal (add_level : in type_add_level) 
				return pac_units_internal.cursor 
			is
			-- Searches for a unit with given add_level. Returns the cursor of that unit.
			-- If no suitable unit found, returns cursor with no_element.
				cursor : pac_units_internal.cursor := device.units_internal.first;
			begin
				while cursor /= pac_units_internal.no_element loop
					if element (cursor).add_level = add_level then
						return cursor; -- unit found, no further search required. exit prematurely.
					end if;
					next (cursor);
				end loop;
				-- no unit found. return no_element:
				return pac_units_internal.no_element;
			end;

			
			function first_external (add_level : in type_add_level) 
				return pac_units_external.cursor 
			is
			-- Searches for a unit with given add_level. Returns the cursor of that unit.
			-- If no suitable unit found, returns cursor with no_element.
				cursor : pac_units_external.cursor := device.units_external.first;
			begin
				while cursor /= pac_units_external.no_element loop
					if element (cursor).add_level = add_level then
						return cursor; -- unit found, no further search required. exit prematurely.
					end if;
					next (cursor);
				end loop;
				-- no unit found. return no_element:
				return pac_units_external.no_element;
			end;

			
		begin -- query_units
			-- First search among the internal units for a MUST-unit:
			cursors.int := first_internal (MUST);

			-- if no MUST-unit found, search for an ALWAYS-unit:
			if cursors.int = pac_units_internal.no_element then
				cursors.int := first_internal (ALWAYS);

				-- if no ALWAYS-unit found, search for a NEXT-unit:
				if cursors.int = pac_units_internal.no_element then
					cursors.int := first_internal (NEXT);

					-- if no NEXT-unit found, search for a REQUEST-unit
					if cursors.int = pac_units_internal.no_element then
						cursors.int := first_internal (REQUEST);

						-- if no REQUEST-unit found, search for a CAN-unit
						if cursors.int = pac_units_internal.no_element then
							cursors.int := first_internal (et_unit_add_level.CAN);
						end if;
					end if;					
				end if;
			end if;

			
			-- if no suitable internal unit found, search among the external units:
			if cursors.int = pac_units_internal.no_element then

				-- search among the external units for a MUST-unit
				cursors.ext := first_external (MUST);

				-- if no MUST-unit found, search for an ALWAYS-unit:
				if cursors.ext = pac_units_external.no_element then
					cursors.ext := first_external (ALWAYS);

					-- if no ALWAYS-unit found, search for a NEXT-unit:
					if cursors.ext = pac_units_external.no_element then
						cursors.ext := first_external (NEXT);

						-- if no NEXT-unit found, search for a REQUEST-unit
						if cursors.ext = pac_units_external.no_element then
							cursors.ext := first_external (REQUEST);

							-- if no REQUEST-unit found, search for a CAN-unit
							if cursors.ext = pac_units_external.no_element then
								cursors.ext := first_external (et_unit_add_level.CAN);
							end if;
						end if;					
					end if;
				end if;
			
				-- if no suitable external unit found, we have a problem:
				if cursors.ext = pac_units_external.no_element then
					log (ERROR, " Device model has no units !", console => true);
					raise constraint_error;
				end if;

			end if;
			
		end query_units;
		
	begin
		query_element (
			position	=> device_cursor,
			process		=> query_units'access);
		
		return cursors;
	end get_first_unit;

	

	function get_first_unit (
		device_cursor : in pac_devices_lib.cursor) 
		return pac_unit_name.bounded_string
	is
		fu : constant type_device_units := get_first_unit (device_cursor);

		use pac_units_internal;
		use pac_units_external;
		
		unit_name : pac_unit_name.bounded_string; -- to be returned
	begin
		-- The first unit is either internal or external.
		
		-- If first unit is an internal one:
		if fu.int /= pac_units_internal.no_element then

			unit_name := key (fu.int);
			
		-- If first unit is an external one:
		elsif fu.ext /= pac_units_external.no_element then
			
			unit_name := key (fu.ext);
			
		else
			raise constraint_error; -- CS should never happen. function first_unit excludes this case.
		end if;

		return unit_name;
	end get_first_unit;



	
	function get_unit (
		device_cursor	: in pac_devices_lib.cursor;
		unit_name		: in pac_unit_name.bounded_string)
		return type_device_units 
	is
		cursors : type_device_units; -- to be returned
		
		use pac_units_internal;
		use pac_units_external;

		
		procedure query_units (
			device_name	: in pac_device_model_file.bounded_string;
			device		: in type_device_model) 
		is begin
			-- First search among the internal units:
			cursors.int := device.units_internal.first;

			while cursors.int /= pac_units_internal.no_element loop
				if key (cursors.int) = unit_name then
					exit; -- unit found, no further search required. exit prematurely.
				end if;
				next (cursors.int);
			end loop;

			-- if no suitable internal unit found, search among the external units:
			if cursors.int = pac_units_internal.no_element then

				cursors.ext := device.units_external.first;

				while cursors.ext /= pac_units_external.no_element loop
					if key (cursors.ext) = unit_name then
						exit; -- unit found, no further search required. exit prematurely.
					end if;
					next (cursors.ext);
				end loop;
				
				-- if no suitable external unit found, we have a problem:
				if cursors.ext = pac_units_external.no_element then
					log (ERROR, "Unit " & to_string (unit_name) &
						 " not defined in device model !", console => true);
					
					raise semantic_error_1 with
						"ERROR: Unit " & to_string (unit_name) &
						 " not defined in device model !";
				end if;

			end if;
			
		end query_units;
		
	begin
		query_element (
			position	=> device_cursor,
			process		=> query_units'access);
		
		return cursors;
	end get_unit;



	
	function get_all_units (
		device_cursor	: in pac_devices_lib.cursor)
		return pac_unit_names.list
	is
		result : pac_unit_names.list; -- to be returned

		use pac_units_internal;
		use pac_units_external;
		
		procedure query_internal (c : in pac_units_internal.cursor) is begin
			result.append (key (c));
		end query_internal;

		procedure query_external (c : in pac_units_external.cursor) is begin
			result.append (key (c));
		end query_external;
									 
	begin
		iterate (element (device_cursor).units_internal, query_internal'access);
		iterate (element (device_cursor).units_external, query_external'access);

		return result;
	end get_all_units;

	

	
	function get_unit_count (
		device_cursor	: in pac_devices_lib.cursor)
		return type_unit_count 
	is begin
		return get_unit_count (element (device_cursor));
	end get_unit_count;


	

	function get_package_variant (
		device_cursor	: in pac_devices_lib.cursor;
		variant			: in pac_package_variant_name.bounded_string)
		return pac_package_variants.cursor
	is 
		result : pac_package_variants.cursor;

		procedure query_variants (
			device_name	: in pac_device_model_file.bounded_string;
			device		: in type_device_model) 
		is 
			use pac_package_variants;
			--vc : constant pac_package_variants.cursor := find (device.variants, variant);
		begin
			result := find (device.variants, variant);
			--if vc /= pac_package_variants.no_element then
				--result := vc;
			--else
				--raise semantic_error_1 with "Package variant " 
					--& enclose_in_quotes (to_string (variant)) &
					--" not defined."; 
					---- CS output model name. Mind max. string length of error message.
			--end if;
		end query_variants;
		
	begin
		query_element (device_cursor, query_variants'access);
		return result;
	end get_package_variant;


	
	
	
	function is_variant_available (
		device_cursor	: in pac_devices_lib.cursor;
		variant			: in pac_package_variant_name.bounded_string)  -- D, N
		return boolean is
		
		result : boolean := false; -- to be returned
		
		procedure query_variants (
			device_name	: in pac_device_model_file.bounded_string;
			device		: in type_device_model) is
		begin
			if pac_package_variants.contains (device.variants, variant) then
				result := true;
			end if;
		end;
		
	begin
		pac_devices_lib.query_element (
			position	=> device_cursor,
			process		=> query_variants'access);
		
		return result;
	end is_variant_available;


	

	function get_available_variants (
		device_cursor	: in pac_devices_lib.cursor)
		return pac_package_variants.map
	is
		result : pac_package_variants.map; -- to be returned
	begin
		case element (device_cursor).appearance is
			when APPEARANCE_PCB		=> result := element (device_cursor).variants;
			when APPEARANCE_VIRTUAL	=> null;
		end case;
		
		return result;
	end get_available_variants;



	


	function get_first_package_variant (
		device_cursor : in pac_devices_lib.cursor)
		return pac_package_variant_name.bounded_string
	is
		device_model : type_device_model renames element (device_cursor);
	begin
		return get_first_package_variant (device_model);
	end;





	function get_default_value (
		device_cursor : in pac_devices_lib.cursor)
		return pac_device_value.bounded_string
	is
		device_model : type_device_model renames element (device_cursor);
	begin
		return get_default_value (device_model);
	end;

	


	
	function locate_unit (
		device_cursor	: in pac_devices_lib.cursor;
		unit_name		: in pac_unit_name.bounded_string) -- like "I/O-Bank 3"
		return type_unit_cursors 
	is
		use pac_units_external;
		use pac_units_internal;

		status : type_unit_ext_int;
		
		cursor_external : pac_units_external.cursor;
		cursor_internal : pac_units_internal.cursor;

		procedure query_units (
			model	: in pac_device_model_file.bounded_string;
			device	: in type_device_model) is
		begin
			-- Most likely the requested unit is external. So we search first in 
			-- the list of external units of the given device:
			cursor_external := find (device.units_external, unit_name);

			-- If the unit has been found, return the cursor to it:
			if cursor_external /= pac_units_external.no_element then
				status := EXT;
			else
			-- If the unit could not be found, it must be an internal unit. Search among
			-- the internal units of the given device:
				cursor_internal := find (device.units_internal, unit_name);
				status := INT;
			end if;
		end;
		
	begin -- locate_unit
		--put_line (to_string (pac_devices_lib.key (device_cursor)));

		query_element (device_cursor, query_units'access);

		case status is
			when EXT => return (EXT, cursor_external);
			when INT => return (INT, cursor_internal);
		end case;

	end locate_unit;




	function get_symbol (
		unit : in type_unit_cursors)
		return et_symbols.type_symbol
	is 
		use pac_units_external;
		use pac_units_internal;
		
		model_name : pac_symbol_model_file.bounded_string;
		-- like libraries/symbols/NAND.sym
		
		symbol_cursor : et_symbols.pac_symbols.cursor;
	begin
		case unit.ext_int is
			when EXT =>
				-- put_line ("external unit");

				-- If the unit is external, we must fetch the symbol 
				-- via its model file:
				model_name := element (unit.external).model;
				locate_symbol (model_name, symbol_cursor);

				return pac_symbols.element (symbol_cursor);

			when INT =>
				-- put_line ("internal unit");

				-- If the unit is internal, we can fetch it the symbol 
				-- directly from the unit:
				return element (unit.internal).symbol;
		end case;
	end get_symbol;



	
	
	function get_package_model (
		device_cursor	: in pac_devices_lib.cursor;
		variant			: in pac_package_variant_name.bounded_string)
		return pac_package_model_file_name.bounded_string 
	is
		package_model : pac_package_model_file_name.bounded_string; -- to be returned (packages/smd/SOT23.pac)
		
		procedure query_variants (
			device_name	: in pac_device_model_file.bounded_string;
			device		: in type_device_model) 
		is
			use pac_package_variants;
			variant_cursor : pac_package_variants.cursor;
		begin
			variant_cursor := pac_package_variants.find (device.variants, variant);
			package_model := element (variant_cursor).package_model;
		end;
		
	begin
		pac_devices_lib.query_element (
			position	=> device_cursor,
			process		=> query_variants'access);

		return package_model;
	end get_package_model;


	

	function get_properties (
		device_cursor	: in pac_devices_lib.cursor;
		port_name		: in pac_port_name.bounded_string)
		return pac_ports.cursor 
	is

		port_cursor : pac_ports.cursor; -- to be returned


		procedure query_units (
			model	: in pac_device_model_file.bounded_string; -- ../libraries/devices/logic_ttl/7400.dev
			device	: in type_device_model) is

			use pac_units_internal;
			unit_internal_cursor : pac_units_internal.cursor := device.units_internal.first;
			
			use pac_units_external;
			unit_external_cursor : pac_units_external.cursor := device.units_external.first;

			use pac_ports;

			
			-- Query ports of internal unit.
			procedure query_ports (
				unit_name	: in pac_unit_name.bounded_string;
				unit		: in type_unit_internal) is
			begin				
				port_cursor := find (unit.symbol.ports, port_name);
			end query_ports;

			
			-- Query ports of external unit.
			procedure query_symbols (
				unit_name	: in pac_unit_name.bounded_string;
				unit		: in type_unit_external) 
			is
				use pac_symbols;
				symbol_cursor : pac_symbols.cursor;

				procedure query_ports (
					file	: in pac_symbol_model_file.bounded_string; -- ../libraries/symbols/NAND.sym
					symbol	: in type_symbol) 
				is begin
					port_cursor := find (symbol.ports, port_name);
				end;
				
			begin
				locate_symbol (unit.model, symbol_cursor);
				
				query_element (
					position	=> symbol_cursor,
					process		=> query_ports'access);
			end query_symbols;
			
			
		begin -- query_units
			-- search the port among the internal units first
			while unit_internal_cursor /= pac_units_internal.no_element loop

				query_element (
					position	=> unit_internal_cursor,
					process		=> query_ports'access);

				-- The search ends when the given port has been found.
				if port_cursor /= pac_ports.no_element then
					exit;
				end if;
				
				next (unit_internal_cursor);
			end loop;

			-- if port not found among the internal units, search in external units:
			if port_cursor = pac_ports.no_element then
				while unit_external_cursor /= pac_units_external.no_element loop

					query_element (
						position	=> unit_external_cursor,
						process		=> query_symbols'access);

					-- The search ends when the given port has been found.
					if port_cursor /= pac_ports.no_element then
						exit;
					end if;
										
					next (unit_external_cursor);
				end loop;
			end if;

		end query_units;

		
	begin
		query_element (
			position	=> device_cursor,
			process		=> query_units'access);
		
		return port_cursor;
	end get_properties;






	function get_package_placeholders (
		device	: in pac_devices_lib.cursor;
		variant	: in pac_package_variant_name.bounded_string) -- N, D, S_0805
		return type_text_placeholders
	is
		use pac_package_variants;
		placeholders		: type_text_placeholders; -- to be returned

		-- fetch the package variants available for the given device:
		variants_available	: pac_package_variants.map := element (device).variants;
		
		variant_cursor		: pac_package_variants.cursor;
		package_model		: pac_package_model_file_name.bounded_string; -- ../lbr/smd/SO15.pac

		use et_packages;		
		use pac_package_models;
		package_cursor		: pac_package_models.cursor;

	begin -- get_package_placeholders
		
		-- locate the given variant in the device:
		variant_cursor := pac_package_variants.find (variants_available, variant);

		-- get the package model name:
		package_model := element (variant_cursor).package_model; -- ../lbr/smd/SO15.pac

		-- locate the package model in the package library:
		package_cursor := pac_package_models.find (package_models, package_model);

		-- fetch the placeholders of silk screen top and bottom
		placeholders.silkscreen.top := element (package_cursor).silkscreen.top.placeholders;
		placeholders.silkscreen.bottom := element (package_cursor).silkscreen.bottom.placeholders;

		-- fetch the placeholders of assembly documentation top and bottom
		placeholders.assy_doc.top := element (package_cursor).assy_doc.top.placeholders;
		placeholders.assy_doc.bottom := element (package_cursor).assy_doc.bottom.placeholders;
		
		return placeholders;
	end get_package_placeholders;







	function get_ports_of_unit (
		device_cursor	: in pac_devices_lib.cursor;
		unit_name		: in pac_unit_name.bounded_string)
		return pac_ports.map 
	is
		ports : pac_ports.map; -- to be returned
		
		
		procedure query_internal_units (
			model	: in pac_device_model_file.bounded_string;
			device	: in type_device_model) 
		is
			unit_cursor : pac_units_internal.cursor;
		begin
			-- locate the given unit among the internal units:
			locate_internal (device, unit_name, unit_cursor);

			-- Fetch the ports of the internal unit.
			ports := get_ports_internal (unit_cursor);
		end query_internal_units;


		
		procedure query_external_units (
			model	: in pac_device_model_file.bounded_string;
			device	: in type_device_model) 
		is
			use pac_units_external;
			unit_cursor : pac_units_external.cursor;
		begin
			-- locate the given unit among the external units:
			locate_external (device, unit_name, unit_cursor);
			
			-- Fetch the symbol model file of the external unit.
			-- If unit could not be located, nothing happens -> ports remains empty.
			if has_element (unit_cursor) then
				ports := get_ports_external (unit_cursor);
			end if;			
		end query_external_units;
		
		
	begin -- get_ports_of_unit

		-- Query external units of the device.
		-- It is most likely that
		-- the unit is among the external units:
		query_element (
			position	=> device_cursor,
			process		=> query_external_units'access);

		-- If the unit could not be found among external units 
		-- then look up the internal units:
		if pac_ports.length (ports) = 0 then

			query_element (
				position	=> device_cursor,
				process		=> query_internal_units'access);
		end if;

		
		-- If still no ports found, then we have a problem:
		if pac_ports.length (ports) = 0 then
			raise constraint_error;
		end if;
		
		return ports;

		-- exception
		-- 	when event: others =>
		-- 		log_indentation_reset;
		-- 		log (text => ada.exceptions.exception_information (event), console => true);
		-- 		raise;
		
	end get_ports_of_unit;


	
	
		
end et_device_library;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
