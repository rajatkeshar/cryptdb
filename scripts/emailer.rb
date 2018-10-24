require 'net/smtp'

FROM = "cryptdb@turkeyday.com"
message = <<MESSAGE_END
From: cryptdb daemon <#{FROM}>
To: cryptdb admins <admin@cryptdb.com>
MIME-Version: 1.0
Content-type: text/html
Subject: double proxy may have crashed [eom]
asdf
MESSAGE_END

FIVE_MINUTES = 60*5
LOG_FILE = File.join(ENV["EDBDIR"], "/logs/double.log")

$previous_size = -1
while true do
    sleep(FIVE_MINUTES)
    current_size = File.size?(LOG_FILE)
    if current_size != $previous_size then
        $previous_size = current_size
        next
    end

    # after five minutes our log hasn't grown, double proxy may be dead
    smtp = Net::SMTP.new('smtp.gmail.com', 587)
    smtp.enable_starttls
    smtp.start('smtp.gmail.com', 'cryptdb', 'upsidedownrainbows',
               :login) {|smtp|
          smtp.send_message(message, FROM,
                            ['cryptdb@gmail.com','burrows.labs@gmail.com',
                             'raluca@csail.mit.edu'])
    }
end
