------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                               GENERAL                                    --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2019 Mario Blunk, Blunk electronic                 --
--                                                                          --
--    This program is free software: you can redistribute it and/or modify  --
--    it under the terms of the GNU General Public License as published by  --
--    the Free Software Foundation, either version 3 of the License, or     --
--    (at your option) any later version.                                   --
--                                                                          --
--    This program is distributed in the hope that it will be useful,       --
--    but WITHOUT ANY WARRANTY; without even the implied warranty of        --
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         --
--    GNU General Public License for more details.                          --
--                                                                          --
--    You should have received a copy of the GNU General Public License     --
--    along with this program.  If not, see <http://www.gnu.org/licenses/>. --
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
with ada.characters;			use ada.characters;
with ada.characters.latin_1;	use ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;
with ada.directories;
with et_string_processing;

package body et_general is
	
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
		return type_directory_entries.list is

		use ada.directories;
		filter : filter_type;
		entries : type_directory_entries.list; -- to be returned
		
		procedure do_it (item : in directory_entry_type) is
		-- appends items to the container "entries"
		begin
			type_directory_entries.append (
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
		dash : constant character := latin_1.hyphen;
	begin
		put_line ("available commandline switches:");
		put_line (dash & switch_version);
		put_line (dash & switch_native_project_create);
		put_line (dash & switch_native_project_open);
		put_line (dash & switch_execute_script);
		put_line (dash & switch_native_project_save_as);
		put_line (dash & switch_make_default_conv);
		put_line (dash & switch_import_project);
		put_line ("For additional switches and examples see <https://github.com/Blunk-electronic/ET>");
	end show_cdl_switches;

	function to_paper_size (paper_size : in string) return type_paper_size is
	-- converts a string to type_paper_size
	begin
		return type_paper_size'value (paper_size);
	end to_paper_size;
	
	function to_string (paper_size : in type_paper_size) return string is
	begin
		return type_paper_size'image (paper_size);
	end to_string;

	
-- NET NAMES
	procedure check_net_name_length (net : in string) is
	-- Tests if the given net name is longer than allowed.	
		use et_string_processing;
	begin
		if net'length > net_name_length_max then
			log (ERROR, "max. number of characters for net name is" 
				 & positive'image (net_name_length_max) & " !",
				 console => true);
			raise constraint_error;
		end if;
	end check_net_name_length;

	procedure check_net_name_characters (
		net			: in type_net_name.bounded_string;
		characters	: in character_set := net_name_characters) is
	-- Tests if the given net name contains only valid characters as specified
	-- by given character set.
		use et_string_processing;
		invalid_character_position : natural := 0;
		inversion_mark_position : natural := 0;
	begin
		-- Test given net name and get position of possible invalid characters.
		invalid_character_position := index (
			source => net,
			set => characters,
			test => outside);

		-- Evaluate position of invalid character.
		if invalid_character_position > 0 then
			log (ERROR, "invalid character in net name '" 
				 & to_string (net) & "' at position" 
				 & natural'image (invalid_character_position) & " !",
				 console => true);

			-- CS: show allowed characters
			raise constraint_error;
		end if;

		-- If there is an inversion mark, it must be at the very end of the net name.
		inversion_mark_position := type_net_name.index (net, net_inversion_mark);
		if inversion_mark_position > 0 then
			if inversion_mark_position /= type_net_name.length (net) then
				log (ERROR, "net " & to_string (net) 
					& " inversion mark must be at the end of the net name !",
					console => true);
				raise constraint_error;
			end if;
		end if;
		
	end check_net_name_characters;
	
	function to_net_name (net_name : in string) return type_net_name.bounded_string is begin
		return type_net_name.to_bounded_string (net_name);
	end to_net_name;
	
	function to_string (net_name : in type_net_name.bounded_string) return string is begin
		return type_net_name.to_string (net_name);
	end to_string;

	function anonymous (net_name : in type_net_name.bounded_string) return boolean is
	-- Returns true if the given net name is anonymous.
	begin
		-- CS: this is just a test if anonymous_net_name_prefix is somewhere
		-- in the net name. Should be improved.
		if type_net_name.count (net_name, anonymous_net_name_prefix) > 0 then
			return true;
		else 
			return false;
		end if;
	end anonymous;

	
	function to_string (name : in type_module_name.bounded_string) return string is begin
		return type_module_name.to_string (name);
	end;

	function to_module_name (name : in string) return type_module_name.bounded_string is begin
		return type_module_name.to_bounded_string (name);
	end;
	

	function to_string (name : in type_module_instance_name.bounded_string) return string is begin
		return type_module_instance_name.to_string (name);
	end;

	function to_instance_name (name : in string) return type_module_instance_name.bounded_string is begin
		return type_module_instance_name.to_bounded_string (name);
	end;
	


	
	function is_default (variant : in type_variant_name.bounded_string) return boolean is begin
	-- Returns true if the given variant name is empty.
		if type_variant_name.length (variant) = 0 then -- CS better compare with constant "default"
			return true;
		else
			return false;
		end if;
	end;

	function to_variant (variant : in type_variant_name.bounded_string) return string is begin
		return type_variant_name.to_string (variant);
	end;

	function to_variant (variant : in string) return type_variant_name.bounded_string is begin
		-- CS lenght and character check
		return type_variant_name.to_bounded_string (variant);
	end;


	function to_shape (shape : in string) return type_shape is begin
		return type_shape'value (shape);
	end;

	function to_shape (shape : in type_shape) return string is begin
		return to_lower (type_shape'image (shape));
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
