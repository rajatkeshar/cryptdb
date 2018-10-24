require 'open3'

def babysit(cmd)
    while true do
        # `#{cmd} 2>&1`
        `#{cmd} 1>&2`
    end
end

babysit("bash -c local_proxy_fn")

