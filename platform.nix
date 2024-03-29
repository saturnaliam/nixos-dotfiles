{ lib, ... }:
{
	options.platform = lib.mkOption {
		description = "plataform";
		type = lib.types.str;
	};
}
