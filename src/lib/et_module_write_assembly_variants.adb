------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                   MODULE WRITE / ASSEMBLY VARIANTS                       --
--                                                                          --
--                               B o d y                                    --
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
--                                                                          --
------------------------------------------------------------------------------

--   For correct displaying set tab with in your edtior to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--
-- ToDo:
-- - clean up
--
--
--

with ada.text_io;					use ada.text_io;
with ada.strings;					use ada.strings;
with ada.characters;				use ada.characters;
with ada.characters.latin_1;

with et_module;						use et_module;
with et_module_names;				use et_module_names;
with et_section_headers;			use et_section_headers;
with et_keywords;					use et_keywords;

with et_module_instance;			use et_module_instance;
with et_device_name;				use et_device_name;
with et_device_model;
with et_device_purpose;
with et_device_value;
with et_device_partcode;
with et_assembly_variants;			use et_assembly_variants;
with et_assembly_variant_name;		use et_assembly_variant_name;

with et_schematic_ops;
with et_schematic_ops.units;		use et_schematic_ops.units;
with et_schematic_ops.submodules;

with et_general_rw;					use et_general_rw;



package body et_module_write_assembly_variants is

	use pac_generic_modules;


	procedure write_assembly_variants (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
	is
		use pac_assembly_variants;

		
		
		procedure query_devices (
			variant_name	: in pac_assembly_variant_name.bounded_string;
			variant			: in type_assembly_variant) 
		is
			use et_device_model;
			use et_device_partcode;
			use et_device_value;
			use pac_device_variants;
			device_cursor : pac_device_variants.cursor := variant.devices.first;

			
			function purpose return string is 
				use et_device_purpose;
			begin
				if get_length (element (device_cursor).purpose) > 0 then
					return space & keyword_purpose & space &
						enclose_in_quotes (
							text_in => to_string (element (device_cursor).purpose),
							quote	=> latin_1.quotation);
				else
					return "";
				end if;
			end;

			
		begin -- query_devices
			while device_cursor /= pac_device_variants.no_element loop
				case element (device_cursor).mounted is
					when NO =>
						write (
							keyword		=> keyword_device,
							parameters	=> to_string (key (device_cursor)) & 
											space & keyword_not_mounted);

					when YES =>
						write (
							keyword		=> keyword_device,
							parameters	=> to_string (key (device_cursor)) & 
								space &
								keyword_value & space &
								to_string (element (device_cursor).value) &
								space & keyword_partcode & space &
								to_string (element (device_cursor).partcode) &
								purpose);

				end case;
				
				next (device_cursor);
			end loop;
		end query_devices;



		
		
		procedure query_submodules (
			variant_name	: in pac_assembly_variant_name.bounded_string;
			variant			: in type_assembly_variant) 
		is
			use et_module_instance;
			use pac_submodule_variants;
			submodule_cursor : pac_submodule_variants.cursor := variant.submodules.first;
		begin
			while submodule_cursor /= pac_submodule_variants.no_element loop
				write (
					keyword		=> keyword_submodule,
					parameters	=> to_string (key (submodule_cursor)) &
									space & keyword_variant & space &
									to_variant (element (submodule_cursor).variant));
				
				next (submodule_cursor);
			end loop;
		end query_submodules;




		
		
		procedure write (variant_cursor : in pac_assembly_variants.cursor) is 
		begin
			section_mark (section_assembly_variant, HEADER);
			write (keyword => keyword_name, parameters => to_variant (key (variant_cursor)));
			write (keyword => keyword_description, wrap => true, parameters => to_string (element (variant_cursor).description));

			-- write the device variants
			query_element (
				position	=> variant_cursor,
				process		=> query_devices'access);

			-- write the submodule variants
			query_element (
				position	=> variant_cursor,
				process		=> query_submodules'access);
			
			section_mark (section_assembly_variant, FOOTER);
			new_line;
		end write;


	

		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is begin
			section_mark (section_assembly_variants, HEADER);

			-- Write assembly variants if such exist for the module.
			-- If no variants exist, then this section will be left empty.
			if get_variant_count (module) > 0 then

				-- iterate assembly variants
				iterate (module.assembly_variants.variants, write'access);

				-- write the active assembly variant
				write (
					keyword		=> keyword_active,
					parameters	=> to_variant (get_active_variant (module)));

			end if;

			
			section_mark (section_assembly_variants, FOOTER);
		end query_module;
		

	
	begin
		log (text => "module " & to_string (module_cursor)
			 & " write assembly variants",
			level => log_threshold);
			
		log_indentation_up;
		
		query_element (module_cursor, query_module'access);
		
	end write_assembly_variants;
	
	
	
	
	
	
end et_module_write_assembly_variants;

	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
