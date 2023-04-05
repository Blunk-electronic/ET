------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                               GENERAL                                    --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
-- Copyright (C) 2017 - 2023                                                -- 
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
-- <http://www.gnu.org/licenses/>.   
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
with ada.strings; 				use ada.strings;
with ada.strings.fixed; 		use ada.strings.fixed;
with ada.characters;			use ada.characters;
with ada.characters.latin_1;	use ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;
with ada.directories;
with gnat.directory_operations;

with et_string_processing;			use et_string_processing;
with et_logging;					use et_logging;


package body et_general is

	function to_string (
		domain : in type_domain) 
		return string 
	is 
		s : string := type_domain'image (domain);
	begin
		return s (domain_prefix'length + 1 .. s'last);
	end to_string;

	
	function to_domain (
		domain : in string) 
		return type_domain 
	is begin
		return type_domain'value (domain_prefix & domain);

		exception when event: others => 
			log (ERROR, "domain " & enclose_in_quotes (domain) & " invalid !", console => true);
			--put_line ("ERROR domain " & domain & " invalid !");
			raise;
	end to_domain;


	
	function expand (
	-- Translates a file name like $HOME/libraries/devices/7400.dev to
	-- /home/user/libraries/devices/7400.dev
	-- CS: works on unix/linux only
		name_in			: in string) -- $HOME/libraries/devices/7400.dev
		--log_threshold	: et_string_processing.type_log_level)
		return string is

		prefix : constant string := ("$"); -- CS: windows ? (like %home%)
		separator : constant string := (1 * gnat.directory_operations.dir_separator); -- /\
		
		place_prefix, place_separator : natural := 0;
		--use gnat.directory_operations;
		--use et_string_processing;
		
-- 		function do_it (path : in string) return string is begin
-- 			log ("full path is " & path, log_threshold + 1);
-- 			return path;
-- 		end;
		
	begin -- expand
		place_prefix := index (name_in, prefix);
		place_separator := index (name_in, separator);

		if place_prefix = 0 then -- no environment variable found
			return name_in; -- return given name as it is
		else
			-- name contains an environment variable
			--log_indentation_up;
			
			--log ("expanding " & name_in, log_threshold);
			-- CS test_vars (name_in); -- test if environment variables exist
			
			--log_indentation_down;
			
			--return do_it (gnat.directory_operations.expand_path (name_in));
			return gnat.directory_operations.expand_path (name_in);
		end if;

	end expand;

	function make_filter_pattern (extension : in string) return string is 
		use ada.directories;
	begin
		return compose (name => "*", extension => extension);
	end make_filter_pattern;

	
	function to_module_file_name (name : in string) return pac_module_file_name.bounded_string is begin
		return pac_module_file_name.to_bounded_string (name);
	end;

	function to_string (name : in pac_module_file_name.bounded_string) return string is begin
		return pac_module_file_name.to_string (name);
	end;


	
	function remove_extension (file_name : in string) return string is
	-- Removes from a string like templates/clock_generator.mod the extension so that
	-- the return would be templates/clock_generator .
		name_length : positive := file_name'length;
		pos_last_character : positive;
	begin
		pos_last_character := name_length - module_file_name_extension'length - 1;
		return file_name (file_name'first .. pos_last_character);
	end remove_extension;

	function append_extension (file_name : in string) return string is
	-- Appends to a string like templates/clock_generator the extension "mod" so that
	-- the return would be templates/clock_generator.mod .
	begin
		return file_name & '.' & module_file_name_extension;
	end;
	
	function directory_entries (
	-- Returns the entries of the given directory. Parameter category determines whether to
	-- search for directories, ordinary files or special files.
		target_directory	: in string;						-- ../lbr
		category			: in ada.directories.file_kind;		-- directory, ordinary_file, special_file
		pattern				: in string) 						-- *.txt
		return pac_directory_entries.list is

		use ada.directories;
		filter : filter_type;
		entries : pac_directory_entries.list; -- to be returned
		
		procedure do_it (item : in directory_entry_type) is
		-- appends items to the container "entries"
		begin
			pac_directory_entries.append (
				container => entries,
				new_item => simple_name (item));
		end do_it;

	begin -- directory_entries

		-- set filter according to the item category:
		case category is
			when directory =>
				filter := (directory => true, others => false);

			when ordinary_file =>
				filter := (ordinary_file => true, others => false);

			when special_file =>
				filter := (special_file => true, others => false);
		end case;

		-- start search
		search (target_directory, pattern, filter, do_it'access);
		
		return entries;
	end directory_entries;

	procedure show_cdl_switches is
	-- Outputs the command line switches that initiate something.
		dash : constant character := hyphen;
	begin
		put_line ("available commandline switches:");
		put_line (dash & switch_version);

		put_line (dash & switch_native_project_create);
		put_line (dash & switch_native_project_open);
		put_line (dash & switch_native_project_save_as);

		put_line (dash & switch_native_package_create);
		put_line (dash & switch_native_package_open);		
		put_line (dash & switch_native_package_save_as);

		put_line (dash & switch_native_symbol_create);
		put_line (dash & switch_native_symbol_open);		
		put_line (dash & switch_native_symbol_save_as);

		put_line (dash & switch_native_device_create);
		put_line (dash & switch_native_device_open);		
		put_line (dash & switch_native_device_save_as);

		put_line (dash & switch_frame_schematic_create);
		put_line (dash & switch_frame_schematic_open);		
		put_line (dash & switch_frame_schematic_save_as);

		put_line (dash & switch_frame_pcb_create);
		put_line (dash & switch_frame_pcb_open);		
		put_line (dash & switch_frame_pcb_save_as);
		
		put_line (dash & switch_execute_script);
		put_line (dash & switch_make_default_conv);
		put_line (dash & switch_import_project);

		put_line (dash & switch_runmode);
		
		put_line ("For additional switches and examples see <https://github.com/Blunk-electronic/ET>");
	end show_cdl_switches;


	
	

	
	function to_string (name : in pac_module_name.bounded_string) return string is begin
		return pac_module_name.to_string (name);
	end;

	function to_module_name (name : in string) return pac_module_name.bounded_string is begin
		return pac_module_name.to_bounded_string (name);
	end;
	

	function to_string (name : in pac_module_instance_name.bounded_string) return string is begin
		return pac_module_instance_name.to_string (name);
	end;

	function to_instance_name (name : in string) return pac_module_instance_name.bounded_string is begin
		return pac_module_instance_name.to_bounded_string (name);
	end;
	



	function to_string (name : in pac_script_name.bounded_string) return string is begin
		return pac_script_name.to_string (name);
	end;
		
	function to_script_name (name : in string) return pac_script_name.bounded_string is begin
		return pac_script_name.to_bounded_string (name);
	end;



-- GENERICS

	
	package body stack_lifo is
		s : array (1..max) of item;
		top : natural range 0..max;

		procedure push(x : item) is
		begin
			top := top + 1;
			s(top) := x;
		end push;

		function pop return item is
		begin
			top := top - 1;
			return s(top + 1);
		end pop;

		function depth return natural is
		begin
			return top;
		end depth;

		procedure init is
		begin
			top := 0;
		end init;
	
	end stack_lifo;

end et_general;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
