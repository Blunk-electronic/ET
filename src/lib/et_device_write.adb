------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                         DEVICE MODEL / WRITE                             --
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

with ada.text_io;						use ada.text_io;
with ada.strings; 						use ada.strings;
with ada.exceptions;

with et_system_info;

with et_string_processing;				use et_string_processing;
with et_time;
with et_file_write;						use et_file_write;

with et_device_partcode;
with et_device_value;
with et_device_prefix;

with et_keywords;						use et_keywords;

with et_device_write_unit;				use et_device_write_unit;
with et_device_write_package_variant;	use et_device_write_package_variant;


package body et_device_write is

	

	procedure write_device (
		file_name		: in pac_device_model_file.bounded_string; -- ../lbr/logic_ttl/7400.dev
		device			: in type_device_model; -- the actual device model
		log_threshold	: in type_log_level)
	is		
		file_handle : ada.text_io.file_type;


		
		procedure write_meta is
			use et_device_prefix;
			use et_device_value;
			use et_device_partcode;
		begin
			log (text => "write meta", level => log_threshold + 1);
			log_indentation_up;
			
			write (keyword => keyword_prefix, parameters => to_string (device.prefix));
			write (keyword => keyword_appearance, parameters => to_string (device.appearance));

			if device.appearance = APPEARANCE_PCB then
			-- CS use function is_real instead
				write (keyword => keyword_value, parameters => to_string (device.value));
				-- CS ? write (keyword => keyword_partcode, parameters => to_string (device.partcode));
			end if;

			log_indentation_down;
		end write_meta;



		
		procedure write_package_variants is
		begin
			log (text => "write package variants", level => log_threshold + 1);
			log_indentation_up;

			if device.appearance = APPEARANCE_PCB then
			-- CS use function is_real instead
				write_package_variant (device.variants, log_threshold + 2);
			end if;

			log_indentation_down;
		end write_package_variants;

		


		procedure write_units is
		begin
			log (text => "write units", level => log_threshold + 1);
			log_indentation_up;
			
			write_internal_units (device.units_internal, log_threshold + 2);
			write_external_units (device.units_external, log_threshold + 2);
			
			log_indentation_down;
		end write_units;


		

		procedure write_header is
			use et_system_info;
			use et_time;
		begin
			-- CS log message
			put_line (comment_mark_default & " " & system_name & " device");
			put_line (comment_mark_default & " " & get_date);
			put_line (comment_mark_default & " " & row_separator_double);
			new_line;
		end write_header;

		

		procedure write_footer is
		begin
			-- CS log message
			new_line;		
			put_line (comment_mark_default & " " & row_separator_double);
			put_line (comment_mark_default & " device model file end");
			new_line;
		end write_footer;
		
		
	begin
		log (text => "save device model as " & to_string (file_name),
			 level => log_threshold);

		log_indentation_up;

		create (
			file 	=> file_handle,
			mode	=> out_file,
			name	=> to_string (file_name));

		set_output (file_handle);

		write_header;

		reset_tab_depth;

		write_meta;
		write_package_variants;
		write_units;

		reset_tab_depth;
		
		set_output (standard_output);
		close (file_handle);

		log_indentation_down;
		-- CS log message
		

		exception when event: others =>
			log_indentation_down;
			log (text => ada.exceptions.exception_message (event));
			if is_open (file_handle) then
				close (file_handle);
			end if;
			raise;
		
	end write_device;

		
end et_device_write;
